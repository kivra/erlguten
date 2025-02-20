%%%-------------------------------------------------------------------
%%% File    : eg_pdf.erl
%%% Author  :  <wright@servicelevel.net>
%%% Description : 
%%%
%%% Created : 25 April 2010 by  <wright@servicelevel.net>
%%%-------------------------------------------------------------------
-module(eg_pdf).

-behaviour(gen_server).

%% API


-define(SERVER, eg_pdf).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3, start_link/1]).



%% Purpose: Generate PDF documents main api

-include("../include/eg.hrl").

-export([add_start_xref/2,
	 add_trailer/4,
	 add_xref/2,
	 append_stream/2,
         begin_text/1,
         bezier/5, bezier/9, bezier_c/4, bezier_v/3, bezier_y/3,
         break_text/1,
         circle/3,
         cms/1,
         color/1,
         default_face/0,
         delete/1,
         ellipse/3,
         end_text/1,
         ensure_font_gets_loaded/2,
         export/1,
         inches/1,
         get_page_no/1,
         get_state/1,
         get_mediabox/1,
         get_string_width/3, get_string_width/4,
         grid/3,
	 header/0,
         image/2, image/3, image/4,
         inBuiltFonts/0,
         kernedtext/2,
         line/2, line/3, line/5,
         lines/2,
         mirror_xaxis/2, mirror_yaxis/2,
         move_to/2,
         new/0,
         new_page/1,
         page_script/2,
         pagesize/1, pagesize/2,
         path/2,
         picas/1,
         points/1,
         poly/2,
         rectangle/3, rectangle/4, rectangle/5, rectangle/6,
         restore_state/1,
         rotate/2,
         round_rect/4,
         round_top_rect/4,
         save_state/1,
         scale/3,
         set_author/2,
         set_char_space/2,
         set_dash/2, set_dash/3,
         set_date/4,
         set_fill_color/2, set_fill_color_CMYK/5, set_fill_color_RGB/4,
         set_fill_gray/2,
         set_font/3,
         set_keywords/2,
         set_line_cap/2,
         set_line_join/2,
         set_line_width/2,
         set_miter_limit/2,
         set_page/2,
         set_pagesize/2, set_pagesize/3,
         set_stroke_color/2, set_stroke_color_CMYK/5, set_stroke_color_RGB/4,
         set_stroke_gray/2,
         set_subject/2,
         set_text_leading/2,
         set_text_pos/3,
         set_text_rendering/2,
         set_text_rise/2,
         set_text_scale/2,
         set_title/2,
         set_word_space/2,
         skew/3,
         svg/2, svg/3, svg/4,
         text/2,
         text_rotate/2,
         text_rotate_position/4,
         textbr/2,
         text_transform/7,
         transform/7, 
	 translate/3,
	 xref/2
        ]).


%% Set up Info, Catalog and Pages

init_pdf_context()->
    {{Year,Month,Day},{Hrs,Min,Sec}} = calendar:local_time(),
    #pdfContext{info=#info{creator="Erlang", 
			   creationDate= {{Year,Month,Day},{Hrs,Min,Sec}},
			   producer="erlguten-3.1", 
			   author="",
			   title="",
			   subject="",
			   keywords="ErlangKeyword"},
		images=dict:new(),
		fonts=[],
		currentpage=1,
		mediabox=pagesize(a4)}.

%% --------------------- User functions --------------

%% @doc Spawn pdf building process
new()->
    {ok, PDF} = start_link( [init_pdf_context(), <<>>] ),
    PDF.

%% @doc Export to PDF file format 
%% return: {PDFDoc::binary(), PageNo::integer()} | exit(Reason)
export(PID)->
  case gen_server:call(PID, {export}, infinity) of
  	{export, PDFDoc, PageNo}->
  	    {PDFDoc, PageNo};
  	{'EXIT', PID, Reason} ->
  	    exit(Reason)
  end.

%% @doc clear up - delete pdf building process
delete(PID)->
    gen_server:cast(PID,{delete}).

%% @doc return the state of the server

get_state(PID) ->
  gen_server:call(PID, {get_state}).

%% @doc return the mediabox set on the server

get_mediabox(PID) ->
  gen_server:call(PID, {get_mediabox}).

%% @doc Add current page context to PDF document and start on a new page 
%% Note page 1 is already created  by default and  current page set 
%% to it after creation of PDF context.
new_page(PID)->
   case gen_server:call( PID, {get_new_page}, infinity) of
    	{page,PageNo}->
    	    PageNo;
    	{'EXIT', PID, Reason} ->
    	    exit(Reason)
    end.



page_script(PID, Script) ->
    gen_server:cast(PID, {page_script, Script}).


%% @doc Go to a page already created.    
set_page(PID, PageNo)->
    gen_server:cast(PID, {page,{set, PageNo}}).

%% @doc Useful for page numbering functions etc.
get_page_no(PID)->
   case gen_server:call( PID, {get_page_no}, infinity) of
    	{page,PageNo}->
    	    PageNo;
    	{'EXIT', PID, Reason} ->
    	    exit(Reason)
    end.
    
%% --- Info -----
%% @doc set the Author atribute of the PDF

set_author(PID,Author)->
      gen_server:cast(PID, {info, {author, Author}} ).
      
%% @doc set the Title atribute of the PDF

set_title(PID,Title)->
    gen_server:cast(PID, {info, {title, Title}} ).
      
%% @doc set the Subject atribute of the PDF

set_subject(PID,Subject)->
    gen_server:cast(PID, {info, {subject, Subject}} ).
      
%% @doc set the Date atribute of the PDF 
   
set_date(PID,Year,Month,Day)->
    gen_server:cast(PID, {info, {date, {Year,Month,Day}}} ).
      
%% @doc set the Keywords atribute of the PDF 
   
set_keywords(PID, Keywords)->
    gen_server:cast(PID, {info, {keywords, Keywords}} ).



%% --- Page ---

%% @doc pagesize returns: bounding box {Xleft, Ybottom, Xright, Ytop}
%%         full pages are always = {0, 0, Width, Height}
pagesize(a0)             -> pagesize( 2380, 3368 );
pagesize(a1)             -> pagesize( 1684, 2380 );
pagesize(a2)             -> pagesize( 1190, 1684 );
pagesize(a3)             -> pagesize( 842, 1190 );
pagesize(a4)             -> pagesize( 595, 842 );
pagesize(a5)             -> pagesize( 421, 595 );
pagesize(a6)             -> pagesize( 297, 421 );
pagesize(a7)             -> pagesize( 210, 297 );
pagesize(a8)             -> pagesize( 148, 210 );
pagesize(a9)             -> pagesize( 105, 148 );
pagesize(b0)             -> pagesize( 2836, 4008 );
pagesize(b1)             -> pagesize( 2004, 2836 );
pagesize(b2)             -> pagesize( 1418, 2004 );
pagesize(b3)             -> pagesize( 1002, 1418 );
pagesize(b4)             -> pagesize( 709, 1002 );
pagesize(b5)             -> pagesize( 501, 709 );
pagesize(b6)             -> pagesize( 355, 501 );
pagesize(b7)             -> pagesize( 250, 355 );
pagesize(b8)             -> pagesize( 178, 250 );
pagesize(b9)             -> pagesize( 125, 178 );
pagesize(b10)            -> pagesize( 89, 125 );
pagesize(c5e)            -> pagesize( 462, 649 );
pagesize(comm10e)        -> pagesize( 298, 683 );
pagesize(dle)            -> pagesize( 312, 624 );
pagesize(executive)      -> pagesize( 542, 720 );
pagesize(folio)          -> pagesize( 595, 935 );
pagesize(ledger)         -> pagesize( 1224, 792 );
pagesize(legal)          -> pagesize( 612, 1008 );
pagesize(letter)         -> pagesize( 612, 792 );
pagesize(tabloid)        -> pagesize( 792, 1224 ).

%% create a full page bounding box for a page of size Width x Height
pagesize(Width, Height) -> {0,0,Width,Height}.

set_pagesize(PID, Size)-> 
  gen_server:cast(PID, {mediabox, pagesize(Size) }).


set_pagesize(PID, Width, Height) -> 
  gen_server:cast(PID, {mediabox, pagesize(Width, Height) }).

%% -- Fonts --

set_font(PID, Fontname, Size)->
    gen_server:cast(PID, {font, {set, Fontname, Size}}).

ensure_font_gets_loaded(PID, FontName) ->
    gen_server:cast(PID, {ensure_font, FontName}).


%% -- This function is a bit expensive, but will stick to the public interface.
get_string_width(_PID, Fontname, PointSize, Str)->
    get_string_width(Fontname, PointSize, Str).

get_string_width(Fontname, PointSize, Str)->
    {richText, Inline} = eg_richText:str2richText(Fontname, PointSize, 
						  0, default, true, Str),
    trunc(lists:foldl(fun(A, Accu) -> eg_richText:width(A) + Accu end, 
		      0, Inline) /1000).

%% units of measure

points(X) -> X.
picas(X) -> X * 6.
inches(X) -> round(X * 72.21).
cms(X) -> round((X * 72.21) / 2.54).

%% @spec color(Color::atom() | {R,G,B}) -> {R,G,B}
%% @doc  R,G,B = 0-255
%%
%%      This may be useful to lookup the rgb value of the color names 
color(Color) ->
    eg_pdf_op:color(Color).

%% Text

begin_text(PID)-> append_stream(PID, eg_pdf_op:begin_text() ).
    
end_text(PID)  -> append_stream(PID, eg_pdf_op:end_text() ).
     
break_text(PID)-> append_stream(PID, eg_pdf_op:break_text() ).
    
text(PID, Text) ->  append_stream(PID, eg_pdf_op:text(Text) ).

textbr(PID,Text)-> append_stream(PID, eg_pdf_op:textbr(Text) ).
    
kernedtext(PID, Text)-> append_stream(PID, eg_pdf_op:kernedtext(Text) ).

set_text_pos(PID, X, Y)->  append_stream(PID, eg_pdf_op:set_text_pos(X,Y)).
		      
set_text_leading(PID, L)-> append_stream( PID, eg_pdf_op:set_text_leading(L) ).


set_text_rendering(PID, MODE) ->
    append_stream(PID, eg_pdf_op:set_text_rendering(MODE) ).


set_char_space(PID, CS) -> append_stream(PID, eg_pdf_op:set_char_space(CS) ).
    
set_word_space(PID, WS) -> append_stream(PID, eg_pdf_op:set_word_space(WS) ).

set_text_scale(PID, SC) -> append_stream(PID, eg_pdf_op:set_text_scale(SC) ).

set_text_rise(PID, RISE)-> append_stream(PID, eg_pdf_op:set_text_rise(RISE)).


%% Graphics operators
path(PID, Type) -> append_stream(PID, eg_pdf_op:path(Type)).
    
move_to(PID,P)-> append_stream(PID, eg_pdf_op:move_to(P) ).

line(PID,From_To)          ->  append_stream(PID, eg_pdf_op:line(From_To) ).
line(PID, From, To)        -> line(PID,{From, To}).
line(PID, X1, Y1, X2, Y2 ) -> line(PID, {{X1,Y1},{X2,Y2}}).

lines(PID, LineList)->  append_stream(PID, eg_pdf_op:lines(LineList)).

%% Poly paths should be stroked/closed/filled with separate
%% command.

poly(PID,Points)->  append_stream(PID, eg_pdf_op:poly(Points)).


%% Grid assumes sorted XLists and YList, minimum value first
grid(PID,XList,YList)->  append_stream(PID, eg_pdf_op:grid(XList,YList)).

%% Bezier paths should be stroked/closed/filled with separate
%% command.


%% @doc bezier/5 (PID,{X1,Y1},{X2,Y2},{X3,Y3},{X4,Y4}) <br/><br/>
%%   This moves to X1,Y1 point as its start and then creates a cubic Bezier curve to X4,Y4 using the points in between as the control points. Bezier paths should be stroked/closed/filled with a separate command.

bezier(PID,{X1,Y1},{X2,Y2},{X3,Y3},{X4,Y4})->
    append_stream(PID, eg_pdf_op:bezier({X1,Y1},{X2,Y2},{X3,Y3},{X4,Y4})).
    
%% @doc bezier/9 (PID,X1,Y1,X2,Y2,X3,Y3,X4,Y4) <br/><br/>
%% This moves to X1,Y1 point as its start and then creates a cubic Bezier curve to X4,Y4 using the points in between as the control points. Bezier paths should be stroked/closed/filled with a separate command.

bezier(PID,X1,Y1,X2,Y2,X3,Y3,X4,Y4)->
    bezier(PID,{X1,Y1},{X2,Y2},{X3,Y3},{X4,Y4}).
    
%% @doc bezier_c/4 (PID,{X1,Y1},{X2,Y2},{X3,Y3}) <br/><br/>
%% This takes the current point as its start and then creates a cubic Bezier curve to Point3 using the points in between as the control points. Bezier paths should be stroked/closed/filled with a separate command.

bezier_c(PID,Point1,Point2,Point3)->
    append_stream(PID, eg_pdf_op:bezier_c(Point1,Point2,Point3)).

%% @doc bezier_v/3 (PID,{X1,Y1},{X2,Y2} ) <br/><br/>
%% This takes the current point as its start and then creates a cubic Bezier curve to Point2 using the current point and Point1 as the control points. Bezier paths should be stroked/closed/filled with a separate command.

bezier_v(PID, Point1, Point2 )->
    append_stream(PID, eg_pdf_op:bezier_v(Point1, Point2)).

%% @doc bezier_y/3  (PID, {X1,Y1},{X3,Y3}) <br/><br/>
%% This takes the current point as its start and then creates a cubic Bezier curve to Point3 using the Point1 and Point3 as the control points. Bezier paths should be stroked/closed/filled with a separate command.

bezier_y(PID, Point1, Point3)->
    append_stream(PID, eg_pdf_op:bezier_y(Point1, Point3)).



circle(PID, {X,Y}, R)->
    append_stream(PID, eg_pdf_op:circle( {X,Y}, R)).

ellipse(PID, {X, Y}, {RX, RY})->
    append_stream(PID, eg_pdf_op:ellipse({X, Y}, {RX, RY})).


%% @doc Stroke a rectangle area.
%% If Stroke Type is not appended in arguments, explicit
%% stroke command "path(StrokeType)" has to be executed.
%%
%% X, Y designate the lower left corner of the rectangle.


rectangle(PID,{X,Y},{WX,WY}) ->
    rectangle(PID,X,Y,WX,WY).

rectangle(PID,{X,Y},{WX,WY}, StrokeType) ->
    rectangle(PID,X,Y,WX,WY,StrokeType).

rectangle(PID,X,Y,WX,WY) when is_pid(PID) ->
    append_stream(PID, eg_pdf_op:rectangle(X,Y,WX,WY)).

rectangle(PID,X,Y,WX,WY,Option) ->
    append_stream(PID, eg_pdf_op:rectangle(X,Y,WX,WY,Option)).

round_rect(PID, Point, Size, Radius)->
    append_stream(PID, eg_pdf_op:round_rect(Point, Size, Radius)).


round_top_rect(PID, Point, Size, Radius)->
    append_stream(PID, eg_pdf_op:round_top_rect(Point, Size, Radius)).


%% Line styles
set_line_width(PID,W)->
    append_stream(PID, eg_pdf_op:set_line_width(W)).

set_line_cap(PID,Mode)->
    append_stream(PID, eg_pdf_op:set_line_cap(Mode)).    

set_line_join(PID, Mode)->
    append_stream(PID, eg_pdf_op:set_line_join(Mode)).    

set_miter_limit(PID,Limit)->
    append_stream(PID, eg_pdf_op:set_miter_limit(Limit)).
        

set_dash(PID, Mode) -> 
    append_stream(PID, eg_pdf_op:set_dash(Mode)).

set_dash(PID, Array, Phase)-> 
    append_stream(PID, eg_pdf_op:set_dash(Array, Phase)).

%% Graphics state
save_state(PID)->
    append_stream(PID, eg_pdf_op:save_state() ).
    
restore_state(PID)->
    append_stream(PID, eg_pdf_op:restore_state() ).

%% Change geometry
transform(PID, A, B, C, D, E, F)->
    append_stream(PID, eg_pdf_op:transform(A, B, C, D, E, F)).
    
%% Change geometry
text_transform(PID, A, B, C, D, E, F)->
    append_stream(PID, eg_pdf_op:text_transform(A, B, C, D, E, F)).

translate(PID, X, Y)->
    append_stream(PID, eg_pdf_op:translate(X,Y)).

scale(PID, ScaleX, ScaleY)->
    append_stream(PID, eg_pdf_op:scale(ScaleX, ScaleY)).

rotate(PID, Angle)->
    append_stream(PID, eg_pdf_op:rotate(Angle)).

text_rotate(PID, Angle)->
    append_stream(PID, eg_pdf_op:text_rotate(Angle)).
    
text_rotate_position(PID, X, Y, Angle)->
    append_stream(PID, eg_pdf_op:text_rotate_position(X, Y, Angle)).

skew(PID, XScewAngle, YScewAngle)->
    append_stream( PID, eg_pdf_op:skew(XScewAngle, YScewAngle) ).

mirror_yaxis(PID,Xtranslate)->
    append_stream(PID, eg_pdf_op:mirror_yaxis(Xtranslate)).

mirror_xaxis(PID,Ytranslate)->
    append_stream(PID, eg_pdf_op:mirror_xaxis(Ytranslate)).

%% Changing colors
%% Color value range 0 - 1
set_fill_color_CMYK(PID,C,M,Y,K)->
    append_stream(PID, eg_pdf_op:set_fill_color_CMYK(C,M,Y,K)).

set_stroke_color_CMYK(PID,C,M,Y,K)->
    append_stream(PID, eg_pdf_op:set_stroke_color_CMYK(C,M,Y,K)).

%% Color value range 0 - 1
set_fill_color_RGB(PID,R,G,B)->
    append_stream(PID, eg_pdf_op:set_fill_color_RGB(R,G,B)).

set_stroke_color_RGB(PID,R,G,B)->
    append_stream(PID, eg_pdf_op:set_stroke_color_RGB(R,G,B)).

%% Color is Name |{R,G,B}, Name = atom(), 0 < R,G,B < 255 
set_fill_color(PID, Color)->
    append_stream(PID, eg_pdf_op:set_fill_color(Color)).

set_stroke_color(PID, Color)->
    append_stream(PID, eg_pdf_op:set_stroke_color(Color)).

    
%% Gray 0.0-Black 1.0-White)
set_fill_gray(PID, Gray)->
    append_stream(PID, eg_pdf_op:set_fill_gray(Gray) ).

set_stroke_gray(PID, Gray)->
    append_stream(PID, eg_pdf_op:set_stroke_gray(Gray)).

%% Images
%% image(PID, FilePath )
%% image(PID, FilePath, Size)
%% image(PID, FilePath, Pos, Size)
%% Pos is {X,Y}
%% Size is {width, W} | {height, H} | {W,H} | {max, W, H} 
%% The max Size version can be used to set a max limit on width, height or both
%% dimensions (undefined is a valid value for at most 1 W or H value)

image(PID, FilePath)->
    save_state(PID),
    case image1(PID, FilePath, {size,{undefined,undefined}}) of
	{error, Reason} -> 
	    {error, Reason};
	ok ->
	    restore_state(PID)
    end.

image(PID, FilePath, Size)->
    save_state(PID),
    case image1(PID, FilePath, Size) of
	{error, Reason} ->
	    {error, Reason};
	ok ->
	    restore_state(PID)
    end.

image(PID, FilePath, {X,Y}, Size)  ->
    save_state(PID),
    translate(PID,X,Y),
    case image1(PID, FilePath, Size) of
	{error, Reason} -> 
	    {error, Reason};
	ok ->
	    restore_state(PID)
    end.

image1(PID, FilePath, {max, undefined, H})->
    image1(PID, FilePath, {height, H});
image1(PID, FilePath, {max, W, undefined})->
    image1(PID, FilePath, {width, W});
image1(PID, FilePath, {max, W, H})->
    image1(PID, FilePath, {size, {max, W, H}});
image1(PID, FilePath, {width, W})->
    image1(PID, FilePath, {size,{W,undefined}});
image1(PID, FilePath, {height, H}) ->
    image1(PID, FilePath, {size,{undefined,H}});
image1(PID, FilePath, {W, H}) when is_integer(W), is_integer(H)->
    image1(PID, FilePath, {size,{W,H}});
image1(PID, FilePath, {size,Size})->
    case file:open(FilePath,[read]) of
	{ok,IO} -> 
	    file:close(IO),
	    gen_server:cast(PID, {image, FilePath, Size});
	{error, OpenError} ->
	    {error, OpenError}
    end.

%% @doc
%% Inserts an SVG as a Path.
%% svg(PID, FilePath)
%% svg(PID, FilePath, Pos)
%% svg(PID, FilePath, Pos, Scale)
%% Pos is {X,Y}
%% Scale is the scale. If the SVG uses 'mm' for the unit of its Height
%% the SVG is scaled to points.

svg(PID, FilePath) ->
    svg(PID, FilePath, {0,0}).

svg(PID, FilePath, {X,Y}) ->
    svg(PID, FilePath, {X,Y}, 1).

svg(PID, FilePath, {X,Y}, Scale) ->
    save_state(PID),
    case svg1(PID, FilePath, {X,Y}, Scale) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            restore_state(PID)
    end.

svg1(PID, FilePath, {X,Y}, Scale) ->
    case eg_svg2pdf:svg2pdf(PID, FilePath, X, Y, Scale) of
        {error, Reason} ->
            {error, Reason};
        Stream ->
            append_stream(PID, Stream)
    end.

%% Internals
append_stream(PID, String)->
    gen_server:cast(PID, {stream, {append, String}}).




%% fontName(Font) ->
%%     case eg_pdf_assemble:fontType(Font) of
%% 	{_,Index} ->
%% 	    Index;
%% 	not_pdf ->
%% 	    io:format("Font:~s is missing using Times-Roman~n", [Font]),
%% 	    1
%%     end.

default_face() ->
    eg_richText:mk_face("Times-Roman", 12, true, default, 0).

%% all_fonts() ->
%%     eg_font_map:all_fonts().

inBuiltFonts() ->
    ["Helvetica","Helvetica-Bold","Helvetica-Oblique",
     "Helvetica-BoldOblique",
     "Times-Roman","Times-Bold","Times-Italic","Times-BoldItalic",
     "Courier","Courier-Bold","Courier-Oblique","Courier-BoldOblique",
     "Symbol", "ZapfDingbats"].


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Init) ->
    gen_server:start_link(?MODULE, Init, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Init) ->
    {ok, Init}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
    	
handle_call({get_page_no}, _From, [PDFC, Stream]) ->	        
	    {reply, {page, PDFC#pdfContext.currentpage}, [PDFC, Stream]};

handle_call({get_new_page}, _From, [PDFC, Stream]) ->	      
	    {Add, PageNo} = 
    		handle_newpage(PDFC#pdfContext.pages,
    			       PDFC#pdfContext.currentpage,
    			       [Stream]),
    {reply, {page, PageNo}, [PDFC#pdfContext{pages=Add, currentpage=PageNo}, <<>>]};
	        
handle_call({export}, _From, [PDFC, Stream]) ->	
	    %% add last page if necessary before exporting
	    PDF = case Stream of 
      		      <<>> ->		    
      			  PageNo = PDFC#pdfContext.pages,
      			  handle_export(PDFC);
      		      _ ->
      			  {Add, PageNo} = handle_newpage(
      					    PDFC#pdfContext.pages,
      					    PDFC#pdfContext.currentpage,
      					    [Stream]),
      			  handle_export(PDFC#pdfContext{pages=Add})
      	    end,
	    {reply, {export, PDF, PageNo}, [PDFC, Stream]};
	    
handle_call({get_state}, _From, [PDFC, Stream]) ->	        
	    {reply, [PDFC, Stream], [PDFC, Stream]};

handle_call({get_mediabox}, _From, [PDFC, Stream]) ->
      {reply, PDFC#pdfContext.mediabox, [PDFC, Stream]}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
	    	    
handle_cast({mediabox, Mediabox}, [PDFC, Stream]) ->	
	    {noreply, [PDFC#pdfContext{mediabox=Mediabox}, Stream]};
	    	    
handle_cast({delete},  [PDFC, Stream]) ->	
	    {stop, normal, [PDFC, Stream]};
	    	    
handle_cast({font, {set, Fontname, Size}}, [PDFC, Stream]) ->	
      {F,Alias,Fhand} = handle_setfont(PDFC#pdfContext.fonts, Fontname),
      S = list_to_binary(eg_pdf_op:set_font_by_alias(Alias, Size)),
      Binary = <<Stream/binary, S/binary>>,
      {noreply, [PDFC#pdfContext{fonts=F,font_handler=Fhand}, Binary]};
      
handle_cast({info,Info}, [PDFC, Stream]) ->	      
	    NewInfo = pdf_handle_info(PDFC#pdfContext.info, Info),
	    {noreply,  [PDFC#pdfContext{info=NewInfo}, Stream]};
	          
handle_cast({stream, {append, String}}, [PDFC, Stream]) ->	    
	    B = list_to_binary(convert(PDFC#pdfContext.font_handler, String)),
	    Binary = <<Stream/binary, B/binary, <<" ">>/binary>>,
	    {noreply, [PDFC, Binary]};
	     
handle_cast({image, FilePath, Size}, [PDFC, Stream]) ->	    
	    {I,IMG,{W,H},ProcSet} = handle_image(PDFC#pdfContext.images, 
						 FilePath, Size, 
						 PDFC#pdfContext.procset),
	    S = list_to_binary(eg_pdf_op:set_image(W,H, IMG)),
	    Binary = <<Stream/binary, S/binary>>,
	    {noreply, [PDFC#pdfContext{images=I,procset=ProcSet}, Binary]};	    
    
handle_cast({page_script, Script}, [PDFC, Stream]) ->	
	    %% io:format("New script ~p\n", [Script]),
	    NewScript = handle_pagescript(PDFC#pdfContext.scripts,
					  PDFC#pdfContext.currentpage,
					  Script),
	    {noreply, [PDFC#pdfContext{scripts=NewScript}, Stream]};

handle_cast({page,{set,PageNo}}, [PDFC, Stream]) ->	
	    {NewPages,[NewStream]} = handle_setpage(PDFC#pdfContext.pages,PageNo,
						  PDFC#pdfContext.currentpage, 
						  [Stream]),
	    {noreply, [PDFC#pdfContext{pages=NewPages,currentpage=PageNo}, NewStream]};	    
  
handle_cast({ensure_font, Fontname}, [PDFC, Stream]) ->	      
	    F = ensure_font( eg_font_map:handler(Fontname), PDFC#pdfContext.fonts),
	    {noreply, [PDFC#pdfContext{fonts=F}, Stream]}.
	    
%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

build_pdf(Info, Fonts, Images, Pages, MediaBox, ProcSet) ->
    %% io:format("build pdf Fonts=~p~n",[Fonts]),
    {Free0,XObjects,O0s}  = eg_pdf_image:mk_images(Images, 1, [], []),
    {Free,Fonts1,O1s}  = mk_fonts(Fonts, Free0, [], []),
    PageTree = Free,
    {Free1,Ps,O3s} = mk_pages(Pages, PageTree, Free+1,[],[]),
    %% io:format("here2:~p~n",[O3s]),
    O2 = {{obj,PageTree,0},
          mkPageTree(Ps, Fonts1, XObjects, MediaBox, ProcSet)},
    Root = Free1,
    O4 = {{obj,Root,0}, mkCatalogue(PageTree)},
    %% io:format("Free1=~p~n",[Free1]),
    NInfo = Free1 + 1,
    O5 = {{obj,NInfo,0}, mkInfo(Info)},
    {Root, NInfo, O0s ++ O1s ++ [O2|O3s] ++ [O4,O5]}.
    
mk_fonts([], I, Fs, Os) -> 
    A = {{obj,I,0},{dict,lists:map(fun({Alias, FontObj}) ->
		      {Alias, {ptr,FontObj,0}}
	      end, lists:reverse(Fs))}},
    {I+1, {ptr,I,0}, lists:reverse([A|Os])};
mk_fonts([Handler|T], I, Fs, E) ->
    %% io:format("I need the font:~p~n",[Handler]),
    Index = Handler:index(),
    Alias = "F" ++ eg_pdf_op:i2s(Index),
    case Handler:type() of
	internal ->
	    O = {{obj,I,0},mkFont(Handler)},
	    mk_fonts(T, I+1, [{Alias,I}|Fs], [O|E]);
	{Index, pdf_builtin} ->
	    O1 = {{obj,I,0},   mkFont1(Handler, I+1, Index)},
	    O2 = {{obj,I+1,0}, mkFontDescriptor(Handler, false, 0)},
	    mk_fonts(T, I+2, [{Alias,I}|Fs], [O2,O1|E]);
	external ->
	    O1 = {{obj,I,0},   mkFont1(Handler, I+1, Index)},
	    O2 = {{obj,I+1,0}, mkFontDescriptor(Handler, true,I+2)},
	    O3 = {{obj,I+2,0}, mkFontFile(Handler)},
	    mk_fonts(T, I+3, [{Alias,I}|Fs], [O3,O2,O1|E])
    end.

mk_pages([], _, N, P, O) -> {N, lists:reverse(P), lists:reverse(O)};
mk_pages([{page,Str}|T], Parent, I, L, E) ->
    O1 = {{obj,I,0},mkPageContents(Str)},
    O2 = {{obj,I+1,0},mkPage( Parent, I)},
    mk_pages(T, Parent, I+2, [I+1|L], [O2,O1|E]);
mk_pages([{page,Str,Script}|T], Parent, I, L, E) ->
    O1 = {{obj,I,0},mkPageContents(Str)},
    O2 = {{obj,I+1,0},mkScript(Script)},
    O3 = {{obj,I+2,0},mkPage( Parent, I, I+1)},
    mk_pages(T, Parent, I+3, [I+2|L], [O3,O2,O1|E]).

mkCatalogue(PageTree) ->
    {dict,[{"Type",{name,"Catalog"}},
	   {"Pages",{ptr,PageTree,0}}]}.

%% mkFont is used for the 14  inbuilt fonts
mkFont(FontHandler) ->
    Index = FontHandler:index(),
    Alias = "F" ++ eg_pdf_op:i2s(Index),
    %% io:format("mkFont Alias=~s FontHandler=~p~n",[Alias, FontHandler]),
    {dict,[{"Type",{name,"Font"}},
	   {"Subtype",{name,"Type1"}},
	   {"Name",{name,Alias}},
	   {"BaseFont",{name,FontHandler:fontName()}},
	   {"Encoding",{name,encoding(FontHandler)}}]}.

encoding(M) ->
    %% Change the encoding to "MacRomanEncoding" except for
    %% "FontSpecific" encodings ...
    %% This seems to work for everything except those fonts
    %% which have a "FontSpecif" encoding.
    %% *usally the encoding in the AFM file is 
    %% "AdobeStandardEncoding" - but this gives an error
    %% for fonts with encoding "AppleStandard". Setting
    %% *everything* to MacRomanEncoding seems to work for all cases
    %% except Zapfdingblats which is "FontSpecific"
    %% - this might not work with files produced on an apple ?
    %% - I have not yet tested this on an apple
    case M:encoding() of
	S = "FontSpecific" ->
	    S;
%   	S = "AppleStandard" ->
%   	    "MacRomanEncoding";
%   	S = "AdobeStandardEncoding" ->
%   	    S;
	_ ->
	    "MacRomanEncoding"
    end.

mkFont1(M, FontDescriptorPrt, Index) ->
    FirstChar = M:firstChar(),
    LastChar = M:lastChar(),
    Widths = make_width(M:encoding(),M,FirstChar,LastChar),
    {dict,[{"Type",{name,"Font"}},
	   {"Subtype",{name,"Type1"}},
	   {"Name",{name,"F" ++ eg_pdf_op:i2s(Index)}},
	   {"BaseFont",{name,M:fontName()}},
	   {"Encoding",{name,encoding(M)}},
	   {"FirstChar",FirstChar},
	   {"LastChar",LastChar},
	   {"Widths", {array,Widths}},
	   {"FontDescriptor",{ptr,FontDescriptorPrt,0}}]}.

make_width("AdobeStandardEncoding", M, F, L) ->
    Seq = lists:seq(F,L),
    Fu = fun(unknown) -> 0;
	   (X) -> X
	end,
    Map = eg_convert:mac2pdf(Seq),
    [Fu(M:width(X)) || X <- Map];
make_width(_, M, _, _) ->
    M:widths().

mkFontDescriptor(M, Embedded, I) ->
    {X1,X2,X3,X4} = M:fontBBox(),
    %% io:format("Flags FIXED to 6 ...~n"),
    FontBBox = [X1,X2,X3,X4],
    D0 = [{"Type",{name,"FontDescriptor"}},
	  {"Ascent", M:ascender()},
	  {"CapHeight", M:capHeight()},
	  {"Descent", M:descender()},
	  {"Flags", M:flags()},
	  {"FontBBox",{array,FontBBox}},
	  {"FontName",{name,M:fontName()}},
	  {"ItalicAngle",M:italicAngle()},
	  {"StemV",M:stemV()},
	  {"XHeight",M:xHeight()}],
    D = case Embedded of
	    true ->
		[{"FontFile", {ptr,I,0}}|D0];
	    false ->
		D0
	end,
    {dict, D}.

%%          {{obj,8,0},
%%           {dict,[{"Type",{name,"FontDescriptor"}},
%%                  {"Ascent",890},
%%                  {"CapHeight",707},
%%                  {"Descent",65306},
%%                  {"Flags",6},
%%                  {"FontBBox",{array,[-100,-65311,1218,895]}},
%%                  {"FontName",{name,"UtopiaMedium"}},
%%                  {"ItalicAngle",0},
%%                  {"StemV",80},
%%                  {"XHeight",512},
%%                  {"FontFile",{ptr,9,0}}]}},
%%          {{obj,9,0},
%%           {stream,94215,
%%                   "stream_9_0_94215",
%%                   [{"Length",94215},
%%                    {"Length1",5750},
%%                    {"Length2",87922},
%%                    {"Length3",543}]}},

mkFontFile(Handler) ->
    {Len,Len1,Len2,Len3,Bin} = get_font_program(Handler),
    {stream,{dict,[{"Length",Len},
		   {"Length1",Len1},
		   {"Length2",Len2},
		   {"Length3",Len3}]},
     Bin}.

this_dir() ->
    filename:dirname(code:which(?MODULE)).

font_dir() ->
    case code:priv_dir(erlguten) of
    %% TODO: Don't think this is correct
	{error, bad_name} ->
        io:format(user,"no priv dir:~n",[]),
	    filename:join(this_dir(), "../priv/fonts");
	N ->
	    filename:join(N, "fonts")
    end.

get_font_program(Handler) ->
    File = filename:join(font_dir(), atom_to_list(Handler) ++ ".pfb"),
    %% io:format(user,"reading Font from:~s~n",[File]),
    P = eg_embed:parse_pfb(File),
    case P of
	[{_,L1,B1},{_,L2,B2},{_,L3,B3}|_] ->
	    {L1+L2+L3,L1,L2,L3,list_to_binary([B1,B2,B3])};
	_ ->
	    error
    end.

mkInfo(I) ->
    {dict,[{"Creator",{string,I#info.creator}},
	   {"CreationDate",{date, I#info.creationDate}},
	   {"Producer",{string,I#info.producer}},
	   {"Author",{string,I#info.author}},
	   {"Title",{string,I#info.title}},
	   {"Subject",{string,I#info.subject}},
	   {"Keywords",{string,I#info.keywords}}]}.

%% L = [int()] = list of objects representing pages


mkPageTree(L, Fonts, XObjects, _MediaBox = {A,B,C,D}, ProcSet ) ->
    ImProcSet = case ProcSet of
		    {imageb,imagec} -> [{name, "ImageB"},{name, "ImageC"}];
		    {imageb,_} -> [{name, "ImageB"}];
		    {_,imagec} -> [{name, "ImageC"}];
		    _ -> []
		end,
    {dict,[{"Type",{name,"Pages"}},
	   {"Count",length(L)},
	   {"MediaBox", {array,[A,B,C,D]}},
	   {"Kids",{array,lists:map(fun(I) ->{ptr,I,0} end,L)}},
	   {"Resources",
	    {dict,[{"Font", Fonts },{"XObject", XObjects },
		   {"ProcSet",
                    {array,[{name,"PDF"},{name,"Text"}|ImProcSet]}}]}}]}.



%% Fonts = [{Name,PageNo}]
%%   example [{"F1",12},{"F7",15}]

mkScript(Script) ->
    {dict,[{"S",{name,"JavaScript"}},
	   {"JS",{string,Script}}]}.


mkPage(Parent, Contents) ->
    {dict, [{"Type", {name,"Page"}},
	    {"Parent", {ptr,Parent,0}},
	    {"Contents", {ptr, Contents, 0}}
	   ]}.

mkPage(Parent, Contents, Script) ->
    {dict, [{"Type", {name,"Page"}},
	    {"Parent", {ptr,Parent,0}},
	    {"Contents", {ptr, Contents, 0}},
	    {"AA", {dict, [{"O", {ptr, Script, 0}}]}}
	   ]}.

% mkPage(Parent, Contents, Script) ->
%     {dict, [{"Type", {name,"Page"}},
% 	    {"Parent", {ptr,Parent,0}},
% 	    {"Contents", {ptr, Contents, 0}},
% 	    {"AA", {dict, [{"O", {ptr, Script, 0}}]}}
% 	   ]}.

mkPageContents(Str) ->
    {stream, Str}.

 
header() ->
    "%PDF-1.3" ++ [8#015,$%,8#342,8#343,8#317,8#323, 8#015,8#012].

%% Objs = {ObjNo, Startpos}

add_xref(F, Objs) ->
    {ok, P} = file:position(F, cur),
    XrefStart = P,
    L  = ["xref\n0 ",eg_pdf_op:i2s(length(Objs)+1),"\n",xref(0,"65535 f")|
	  lists:map(fun({_I,Pos}) -> xref(Pos,"00000 n") end, Objs)],
    file:write(F, L),
    XrefStart.

xref(I, Str) ->
    S = lists:flatten(io_lib:format("~10.10.0w", [I])),
    [S," ", Str,"\r\n"].


add_trailer(F, Objs, Root, Info) ->
    L = ["trailer << /Size ", eg_pdf_op:i2s(length(Objs)+1),
	 " /Root ",eg_pdf_op:i2s(Root), " 0 R ",
	 " /Info ",eg_pdf_op:i2s(Info), " 0 R >>\n"],
    file:write(F, L).

add_start_xref(F, XrefStartPos) ->
    L = ["startxref\n",eg_pdf_op:i2s(XrefStartPos),"\n%%EOF\n"],
    file:write(F, L).


%% xref
%% 0 9
%% 0000000000 65535 f 
%% 0000000033 00000 n 
%% 0000000098 00000 n 
%% 0000000144 00000 n 
%% 0000000203 00000 n 
%% 0000000231 00000 n 
%% 0000000409 00000 n 
%% 0000000721 00000 n 
%% 0000000835 00000 n 
%% trailer
%% <<
%% /Size 9
%% /Root 1 0 R
%% /Info 8 0 R
%% >>
%% startxref
%% 1073
%% %%EOF

%% Internals

handle_pagescript(ScriptDict, PageNo, Script)->
    orddict:store(PageNo,Script,ScriptDict).

handle_setpage(Pages, PageNo, Current, Stream)->
    NewPageDict = orddict:store(Current,Stream,Pages),
    NewStream = orddict:fetch(PageNo, NewPageDict),
    {NewPageDict,NewStream}.

handle_newpage(Pages,0,_Stream)->
    {Pages,1};
handle_newpage(Pages, Current, Stream )->
    NewPageDict = orddict:store(Current,Stream, Pages),
    {NewPageDict, orddict:size(NewPageDict)+1}.

handle_export(PDFC)->
    %% io:format("~nHere handle_export:~p~n",[PDFC]),
    MF = fun(_K,V1,V2) ->
		 {script, V1, V2}
	 end,
    Merged = orddict:merge(MF, PDFC#pdfContext.pages,
			   PDFC#pdfContext.scripts),
    Pages =  lists:map(fun({_Key,{script,Val,S}}) ->
			       {page, Val, S};
			  ({_Key, Val}) ->
			       {page, Val}
		       end,
		       Merged),
    {_Root, Ninfo, Os} = 
	build_pdf(PDFC#pdfContext.info, 
		  PDFC#pdfContext.fonts,
		  dict:to_list(PDFC#pdfContext.images),
		  Pages,
		  PDFC#pdfContext.mediabox,
		  PDFC#pdfContext.procset),
    eg_pdf_lib:export(Ninfo, Os).
%%    Objs = lists:map(fun(I) -> serialise2bin(I) end , Os),
%%    eg_pdf_export:mkdoc(Objs, Root, Ninfo).

%% handle_setfont(FontList, FontName) ->
%%   {FontList1, Alias}
%% Alias = "F" ++ Index
%% alias is a name used in the PDF file to refer to the font.
%%

handle_setfont(FontList, FontName)->
    case eg_font_map:handler(FontName) of
	undefined ->
	    io:format("There is no font called:~s~n", [FontName]),
	    io:format("Using Times-Roman~n"),
	    handle_setfont(FontList, "Times-Roman");
	Handler ->
	    Index = Handler:index(),
	    %% io:format("handler for ~s is ~p index:~p~n",
	    %% [FontName,Handler,Index]),
		  {ensure_font(Handler,FontList), "F"++ eg_pdf_op:i2s(Index), Handler}
    end.
    
ensure_font(Handler, FontList) ->
	    case lists:member(Handler, FontList) of
		true ->
		    FontList;
		false ->
		    [Handler|FontList]
	    end.
	    
%% @doc  This updates the image dictionary from the pdfContext.images with this new image if
%% it's not already present. It also scales the image information to to fit the maximum
%% sizes received in the Size parameter. This may be {undefined,Height}, {Width, undefined} or {max, width, height}.
%% Filepath is the key into the dictionary. If a dictionary entry already exists for the FIlepath
%% it doesn't put it into the dictionary again, but it does calculate the bounding box for the image.
%% When the number of color components is less than or equal to 2, the Procset has a tuple value
%% of {A,B} where A can be undefined or imageb and B can be undefined or imagec. These cause the 
%% listing of these procedure set in the PDf so that the related procedure set can be loaded in 
%% the Postscript printing device. This is suppoed to be obsolete as of v. 1.4 PDFs. 

handle_image(ImageDict, FilePath, Size, ProcSet)->
    case dict:find(FilePath, ImageDict) of
	{ok, #image{alias=Alias, width=W, height=H}} ->
	    {ImageDict, Alias, set_size(Size,{W,H}), ProcSet };
	error ->
	    Alias = "Im" ++ eg_pdf_op:i2s(dict:size(ImageDict) + 1),
	    case eg_pdf_image:get_head_info(FilePath) of
		{jpeg_head,{W1, H1, Ncomponents, _Data_precision}} ->
		    NewDict =dict:store(FilePath,
					#image{alias  = Alias,
                                               width  = W1,
                                               height = H1},
					ImageDict),
		    {NewDict, Alias, set_size(Size, {W1,H1}),
		     imageBC(Ncomponents, ProcSet) };
		{png_head,{W1, H1, Ncomponents, _Data_precision}} ->
		    NewDict =dict:store(FilePath,
					#image{alias  = Alias,
                                               width  = W1,
                                               height = H1},
					ImageDict),
		    {NewDict, Alias, set_size(Size, {W1,H1}),
		     imageBC(Ncomponents, ProcSet) };
		
		A -> 
		    {error_not_yet_implemented_image_format,A}
	    end
    end.

%% Function to scale the image properly if only width or height
%% is set.
set_size({max, W1, H1}, {W2,H2}) -> 
    H3 = trunc(W1*H2/W2),
    W3 = trunc(H1*W2/H2),
    if H3 > H1 ->
	    {W3, H1};
       true ->
	    {W1, H3}
    end;
set_size({undefined,undefined},Size2) -> Size2;
set_size({W1,undefined},{W2,H2}) -> {W1,trunc(W1*H2/W2)};
set_size({undefined,H1},{W2,H2}) -> {trunc(H1*W2/H2),H1};
set_size(Size1,_) -> Size1.

%% @doc Set the image types for ProcSet. If we have black/white image we set imageb; color then imagec. Both can be set.
imageBC(Ncomp,{_B,C}) when Ncomp =< 2 -> {imageb,C};
imageBC(Ncomp,{B,_C}) when Ncomp > 2 -> {B,imagec}.



pdf_handle_info(I,{author,Author})->
    I#info{author=Author};
pdf_handle_info(I,{title,Title}) ->
    I#info{title=Title};
pdf_handle_info(I,{subject,Subject}) ->
    I#info{subject=Subject};
pdf_handle_info(I,{date,{Year,Month,Day}})->
    I#info{creationDate={Year,Month,Day}};
pdf_handle_info(I,{keywords,Keywords}) ->
    I#info{keywords=Keywords}.



convert(undefined,S) -> 
    eg_convert:mac2pdf(S);
convert(Mod, S) ->
    case Mod:encoding() of
	"FontSpecific" ->
	    S;
	_ ->
	    eg_convert:pdf2mac(S)
    end.
