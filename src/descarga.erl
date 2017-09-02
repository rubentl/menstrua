%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (descarga).
-export([main/0]).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> 
    %% wf:content_type("application/pdf"),
    %% wf:download_as("calendario.pdf"),
    %% wf:content_type("text/plain"),
    %% wf:download_as("calendario.txt"),
    elPdf().

elPdf()->
    %% {ok, Binario} = file:read_file("datos.txt"),
    %% Binario.
    %% filelib:wildcard("./**/datos.txt").
    upload:archivo().
