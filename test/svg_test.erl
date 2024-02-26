-module(svg_test).
-include_lib("eunit/include/eunit.hrl").

run_test()->
    ?debugMsg("Begin Test"),
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF, a4),
    eg_pdf_lib:showGrid(PDF, a4),
    SvgFilepath1 = filename:join([code:priv_dir(erlguten), "..", "test",
                                  "images", "abc.svg"]),
    eg_pdf:svg(PDF, SvgFilepath1, {25, 750}),
    eg_pdf:svg(PDF, SvgFilepath1, {100, 600}, 4),
    {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file("./svg_test.pdf",[Serialised]),
    eg_pdf:delete(PDF).
