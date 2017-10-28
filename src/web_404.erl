%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (web_404).
-export([main/0, title/0, body/0, event/1]).

-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Error: Ooops, página no encontrada".

body() -> 
    [#h1{text="Lo siento, hubo algún error.", class="titulo center-align"},
     #h2{text="Por favor, vuelve al inicio", class="subtitulo center-align"},
     #panel{class="main", body=[
        #panel{class="botones center-align", body=[
            #link{text="Volver", postback=home}
            ]}
        ]}
    ].
	
event(home) ->
    wf:redirect("/").
