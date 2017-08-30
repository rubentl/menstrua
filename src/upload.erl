%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (upload).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Hello from upload.erl!".

body() -> 
    [
        #panel { style="margin: 50px 100px;", body=[
            #span { text="Hello from upload.erl!" },

            #p{},
            #button { text="Click me!", postback=click },

            #p{},
            #panel { id=placeholder }
        ]}
    ].
	
event(click) ->
    wf:insert_top(placeholder, "<p>You clicked the button!").
