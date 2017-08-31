%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (descarga).
-export([main/0, title/0, body/0]).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Descarga el calendario menstrual".

body() -> 
    wf:content_type("application/pdf"),
    wf:download_as("Calendario.pdf"),
    convertir(wf:q("archivo")).

convertir(FileName)->
    ok.
