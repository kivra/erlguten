-module(eg_svg2pdf).
-export([svg2pdf/5]).

svg2pdf(_PID, FilePath, X, Y, Scale) ->
    Svg = eg_xml_lite:parse_file(FilePath),
    convert(Svg, [], #{posx => X, posy => Y, scale => Scale}).

convert([{pi, _Encoding}|T], Acc, Ctx) ->
    convert(T, Acc, Ctx);
convert([{comment, _}|T], Acc, Ctx) ->
    convert(T, Acc, Ctx);
convert([{xml, {svg, Header, Svg}}|T], Acc, Ctx0) ->
    Op = flip_and_size(Header, Ctx0),
    {Res, _Ctx1} = convert_svg(Svg, [], Ctx0),
    convert(T, [Res, Op | Acc], Ctx0);
convert([], Acc, _Ctx) ->
    lists:reverse(Acc).

flip_and_size(Header, Ctx) ->
    PosX = maps:get(posx, Ctx, 0),
    PosY = maps:get(posy, Ctx, 0),
    Scale = maps:get(scale, Ctx, 1),
    ViewBox = proplists:get_value("viewBox", Header),
    HeightStr = proplists:get_value("height", Header),
    {ok, {[0, 0, _Width, Height], _}} = get_floats(4, ViewBox),
    {ok, {[_], HeightUnit}} = get_floats(1, HeightStr),
    case HeightUnit of
        "" ->
            [eg_pdf_op:translate(PosX, PosY),
             eg_pdf_op:mirror_xaxis(Scale*Height),
             eg_pdf_op:scale(Scale, Scale)
            ];
        "mm" ->
            %% Scale from mm to points
            [eg_pdf_op:translate(PosX, PosY),
             eg_pdf_op:mirror_xaxis(Scale*Height*2.83465),
             eg_pdf_op:scale(Scale*2.83465, Scale*2.83465)
            ]
    end.

convert_svg([{path, Path, _}|T], Acc, Ctx) ->
    Save = eg_pdf_op:save_state(),
    {Style, Ctx1} = style(Path, Ctx),
    %% string:to_float will convert "0,0" to just {0, []} and not
    %% {0, ",0"}. So we replace , with space.
    D = no_commas(proplists:get_value("d", Path)),
    {PathStr, _Ctx2} = convert_path(D, [], Ctx1),
    Restore = eg_pdf_op:restore_state(),
    convert_svg(T, [Restore, PathStr, Style, Save | Acc], Ctx1#{x => 0, y => 0});
convert_svg([{_Tag, _, []}|T], Acc, Ctx) ->
    convert_svg(T, Acc, Ctx);
convert_svg([{g, Headers, Children}|T], Acc0, Ctx0) ->
    Save = eg_pdf_op:save_state(),
    Transform = transform(proplists:get_value("transform", Headers)),
    {Acc1, Ctx1} = convert_svg(Children, [], Ctx0),
    Restore = eg_pdf_op:restore_state(),
    convert_svg(T, [Restore, Acc1, Transform, Save | Acc0], Ctx1);
convert_svg([{_Tag, _, Children}|T], Acc0, Ctx0) ->
    {Acc1, Ctx1} = convert_svg(Children, [], Ctx0),
    convert_svg(T, [Acc1 | Acc0], Ctx1);
convert_svg([{_, _}], Acc, Ctx) ->
    {lists:reverse(Acc), maps:with([x, y], Ctx)};
convert_svg([], Acc, Ctx) ->
    {lists:reverse(Acc), maps:with([x, y], Ctx)}.

convert_path([$M|Path], Acc, Ctx0) ->
    {ok, {[X, Y], Rest}} = get_floats(2, Path),
    Op = eg_pdf_op:move_to({X, Y}),
    Ctx1 = set_start(Ctx0, X, Y),
    convert_path(Rest, [Op | Acc], Ctx1#{x => X, y => Y, last => "L"});
convert_path([$m|Path], Acc, #{x := X0, y := Y0} = Ctx0) ->
    {ok, {[X1, Y1], Rest}} = get_floats(2, Path),
    X = X1 + X0,
    Y = Y1 + Y0,
    Op = eg_pdf_op:move_to({X, Y}),
    Ctx1 = set_start(Ctx0, X, Y),
    convert_path(Rest, [Op | Acc], Ctx1#{x => X, y => Y, last => "l"});
convert_path([$L|Path], Acc, Ctx) ->
    {ok, {[X, Y], Rest}} = get_floats(2, Path),
    Op = eg_pdf_op:line_append({X, Y}),
    convert_path(Rest, [Op | Acc], Ctx#{x => X, y => Y, last => "L"});
convert_path([$l|Path], Acc, #{x := X0, y := Y0} = Ctx) ->
    {ok, {[X1, Y1], Rest}} = get_floats(2, Path),
    X = X1 + X0,
    Y = Y1 + Y0,
    Op = eg_pdf_op:line_append({X, Y}),
    convert_path(Rest, [Op | Acc], Ctx#{x => X, y => Y, last => "l"});
convert_path([$H|Path], Acc, #{y := Y} = Ctx) ->
    {ok, {[X], Rest}} = get_floats(1, Path),
    Op = eg_pdf_op:line_append({X, Y}),
    convert_path(Rest, [Op | Acc], Ctx#{x => X, last => "H"});
convert_path([$h|Path], Acc, #{x := X0, y := Y} = Ctx) ->
    {ok, {[X1], Rest}} = get_floats(1, Path),
    X = X1 + X0,
    Op = eg_pdf_op:line_append({X, Y}),
    convert_path(Rest, [Op | Acc], Ctx#{x => X, last => "h"});
convert_path([$V|Path], Acc, #{x := X} = Ctx) ->
    {ok, {[Y], Rest}} = get_floats(1, Path),
    Op = eg_pdf_op:line_append({X, Y}),
    convert_path(Rest, [Op | Acc], Ctx#{y => Y, last => "V"});
convert_path([$v|Path], Acc, #{x := X, y := Y0} = Ctx) ->
    {ok, {[Y1], Rest}} = get_floats(1, Path),
    Y = Y1 + Y0,
    Op = eg_pdf_op:line_append({X, Y}),
    convert_path(Rest, [Op | Acc], Ctx#{y => Y, last => "v"});
convert_path([$C|Path], Acc, Ctx) ->
    {ok, {[X1, Y1, X2, Y2, X3, Y3], Rest}} = get_floats(6, Path),
    Op = eg_pdf_op:bezier_c({X1, Y1}, {X2, Y2}, {X3, Y3}),
    convert_path(Rest, [Op | Acc], Ctx#{x => X3, y => Y3, last => "C"});
convert_path([$c|Path], Acc, #{x := X0, y := Y0} = Ctx) ->
    {ok, {[XA, YA, XB, YB, XC, YC], Rest}} = get_floats(6, Path),
    X1 = XA + X0,
    X2 = XB + X0,
    X3 = XC + X0,
    Y1 = YA + Y0,
    Y2 = YB + Y0,
    Y3 = YC + Y0,
    Op = eg_pdf_op:bezier_c({X1, Y1}, {X2, Y2}, {X3, Y3}),
    convert_path(Rest, [Op | Acc], Ctx#{x => X3, y => Y3, last => "c"});
convert_path([$Q|Path], Acc, #{x := X0, y := Y0} = Ctx) ->
    {ok, {[U1, V1, X3, Y3], Rest}} = get_floats(4, Path),
    X1 = X0 + (2/3) * (U1 - X0),
    Y1 = Y0 + (2/3) * (V1 - Y0),
    X2 = X3 + (2/3) * (U1 - X3),
    Y2 = Y3 + (2/3) * (V1 - Y3),
    Op = eg_pdf_op:bezier_c({X1, Y1}, {X2, Y2}, {X3, Y3}),
    convert_path(Rest, [Op | Acc], Ctx#{x => X3, y => Y3, last => "Q"});
convert_path([$q|Path], Acc, #{x := X0, y := Y0} = Ctx) ->
    {ok, {[UA, VA, XC, YC], Rest}} = get_floats(4, Path),
    U1 = UA + X0,
    X3 = XC + X0,
    V1 = VA + Y0,
    Y3 = YC + Y0,
    X1 = X0 + (2/3) * (U1 - X0),
    Y1 = Y0 + (2/3) * (V1 - Y0),
    X2 = X3 + (2/3) * (U1 - X3),
    Y2 = Y3 + (2/3) * (V1 - Y3),
    Op = eg_pdf_op:bezier_c({X1, Y1}, {X2, Y2}, {X3, Y3}),
    convert_path(Rest, [Op | Acc], Ctx#{x => X3, y => Y3, last => "q"});
convert_path([$Z|Path], Acc, #{xs := X, ys := Y} = Ctx0) ->
    Op = eg_pdf_op:path(close),
    Ctx1 = del_start(Ctx0),
    convert_path(chomp_space(Path), [Op | Acc],
                 maps:without([last], Ctx1#{x => X, y => Y}));
convert_path([$z|Path], Acc, #{xs := X, ys := Y} = Ctx0) ->
    Op = eg_pdf_op:path(close),
    Ctx1 = del_start(Ctx0),
    convert_path(chomp_space(Path), [Op | Acc],
                 maps:without([last], Ctx1#{x => X, y => Y}));
%% Odd case of path like this seen:
%% "M 70.828273,52.839833 65.47671,67.351551 h 10.722657 z
convert_path([N|_] = Path, Acc, #{last := Last} = Ctx)
  when N == $- orelse N >= $0 andalso N =< $9 ->
    convert_path(Last ++ Path, Acc, Ctx);
convert_path([], Acc, #{fill_rule := FillRule} = Ctx) ->
    Op = eg_pdf_op:path(FillRule),
    {lists:reverse([Op | Acc]), maps:with([x, y], Ctx)}.

transform(undefined) ->
    [];
transform(TransStr) ->
    Tokens = string:tokens(TransStr, " "),
    parse_transforms(Tokens, []).

parse_transforms(["translate("++Rest | Tail], Acc) ->
    Values = no_commas(Rest),
    {ok, {[X, Y], _}} = get_floats(2, Values),
    Op = eg_pdf_op:translate(X, Y),
    parse_transforms(Tail, [Op | Acc]);
parse_transforms(["rotate("++Rest | Tail], Acc) ->
    Values = no_commas(Rest),
    {ok, [Angle], _} = get_floats(1, Values),
    Op = eg_pdf_op:rotate(Angle),
    parse_transforms(Tail, [Op | Acc]);
parse_transforms(["scale("++Rest | Tail], Acc) ->
    case string:tokens(Rest, ",") of
        [XStr, YStr] ->
            {ok, {[X], _}} = get_floats(1, XStr),
            {ok, {[Y], _}} = get_floats(1, YStr),
            Op = eg_pdf_op:scale(X, Y),
            parse_transforms(Tail, [Op | Acc]);
        [XStr] ->
            {ok, {[X], _}} = get_floats(1, XStr),
            Op = eg_pdf_op:scale(X, X),
            parse_transforms(Tail, [Op | Acc])
    end;
parse_transforms([], Acc) ->
    lists:reverse(Acc).

style(Path, Ctx) ->
    case proplists:get_value("style", Path) of
        undefined ->
            FillColor = proplists:get_value("fill", Path, "none"),
            StrokeColor = proplists:get_value("stroke", Path, "none"),
            FillRule = proplists:get_value("fill-rule", Path),
            Line = line_style(Path, []),
            stroke_and_fill(FillColor, StrokeColor, FillRule, Line, Ctx);
        StyleStr ->
            Style = split_style(StyleStr),
            FillColor = proplists:get_value("fill", Style, "none"),
            StrokeColor = proplists:get_value("stroke", Style, "none"),
            FillRule = proplists:get_value("fill-rule", Style),
            Line = line_style(Style, []),
            stroke_and_fill(FillColor, StrokeColor, FillRule, Line, Ctx)
    end.

split_style(StyleStr) ->
    Tokens = string:tokens(StyleStr, ";"),
    MakeProp = fun(Token) ->
                       [K, V] = string:tokens(Token, ":"),
                       {K, V}
               end,
    lists:map(MakeProp, Tokens).

%% TODO: clean out old stroke colors and such if they are not set here.
stroke_and_fill("none", "none", _, Line, Ctx) ->
    {[Line], Ctx#{fill_rule => stroke_fill_rule(undefined, undefined, undefined)}};
stroke_and_fill(FillColor, "none", FillRule, Line, Ctx) ->
    {R,G,B} = get_color(FillColor),
    {[Line, eg_pdf_op:set_fill_color_RGB(R/255, G/255, B/255)],
     Ctx#{fill_rule => stroke_fill_rule(FillColor, undefined, FillRule)}};
stroke_and_fill("none", StrokeColor, FillRule, Line, Ctx) ->
    {R,G,B} = get_color(StrokeColor),
    {[Line, eg_pdf_op:set_stroke_color_RGB(R/255, G/255, B/255),
     eg_pdf_op:set_dash(solid)],
     Ctx#{fill_rule => stroke_fill_rule(undefined, StrokeColor, FillRule)}};
stroke_and_fill(FillColor, StrokeColor, FillRule, Line, Ctx) ->
    {R1,G1,B1} = get_color(FillColor),
    {R2,G2,B2} = get_color(StrokeColor),
    {[Line, eg_pdf_op:set_fill_color_RGB(R1/255, G1/255, B1/255),
      eg_pdf_op:set_stroke_color_RGB(R2/255, G2/255, B2/255),
      eg_pdf_op:set_dash(solid)],
     Ctx#{fill_rule => stroke_fill_rule(FillColor, StrokeColor, FillRule)}}.

line_style([{"stroke-width", WidthStr} | T], Acc) ->
    {ok, {[W], _}} = get_floats(1, WidthStr),
    Op = eg_pdf_op:set_line_width(W),
    line_style(T, [Op | Acc]);
line_style([{"stroke-linecap", LineCapStr} | T], Acc) ->
    LineCap = line_cap(LineCapStr),
    Op = eg_pdf_op:set_line_cap(LineCap),
    line_style(T, [Op | Acc]);
line_style([{"stroke-linejoin", LineJoinStr} | T], Acc) ->
    LineJoin = line_join(LineJoinStr),
    Op = eg_pdf_op:set_line_join(LineJoin),
    line_style(T, [Op | Acc]);
line_style([_ | T], Acc) ->
    line_style(T, Acc);
line_style([], Acc) ->
    lists:reverse(Acc).

line_cap("butt") -> flat_cap;
line_cap("round") -> round_cap;
line_cap("square") -> square_cap.

line_join("miter") -> miter_join;
line_join("round") -> round_join;
line_join("bevel") -> bevel_join.

get_color([$#, R1, R2, G1, G2, B1, B2]) ->
    R = list_to_integer([R1, R2], 16),
    G = list_to_integer([G1, G2], 16),
    B = list_to_integer([B1, B2], 16),
    {R,G,B}.

stroke_fill_rule(undefined, undefined, undefined) -> endpath;
stroke_fill_rule(undefined, _, _) -> stroke;
stroke_fill_rule(_, undefined, "evenodd") -> fill_even_odd;
stroke_fill_rule(_, undefined, _) -> fill;
stroke_fill_rule(_, _, "evenodd") -> fill_stroke_even_odd;
stroke_fill_rule(_, _, _) -> fill_stroke.

get_floats(N, Rest) ->
    get_floats(N, chomp_space(Rest), []).

get_floats(0, Rest, Acc) ->
    {ok, {lists:reverse(Acc), Rest}};
get_floats(N, Str, Acc) ->
    case string:to_float(Str) of
        {error, Reason} ->
            case string:to_integer(Str) of
                {error, Reason} ->
                    error({Reason, Str});
                {I, Rest} ->
                    get_floats(N - 1, chomp_space(Rest), [I | Acc])
            end;
        {I, Rest} ->
            get_floats(N - 1, chomp_space(Rest), [I | Acc])
    end.

chomp_space([$  | T]) ->
    chomp_space(T);
chomp_space([$, | T]) ->
    chomp_space(T);
chomp_space(Str) ->
    Str.

no_commas(Str) ->
    lists:flatten(string:replace(Str, ",", " ", all)).

set_start(#{xs := _, ys := _} = Ctx, _, _) -> Ctx;
set_start(Ctx, X, Y) -> Ctx#{xs => X, ys => Y}.

del_start(Ctx) ->
    maps:without([xs, ys], Ctx).
