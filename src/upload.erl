%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (upload).
-export([main/0, title/0, body/0, event/1, finish_upload_event/4,
         start_upload_event/1, archivo/0]).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> 
    case wf:user() of 
        auth -> #template { file="./site/templates/bare.html" };
        undefined -> wf:redirect("/")
    end.

title() -> "Carga el archivo con las fechas".

body() -> 
    [
        #h1{text="Archivo con las fechas", class="titulo center-align"},
        #flash{},
        fechas()
    ].

fechas() -> 
    [
        #panel{class="main", body=[
                #panel{class="botones", body=[
                    #upload{
                        tag=archivo, file_text="Selecciona el archivo",
                        show_button=false
                    }
                ]},
                #panel{class="botones center-align", body=[
                    #link{text="Cancelar", url="/"}
                ]}
        ]}
    ].
	
event(_) ->
    ok.

start_upload_event(_) ->
    ok.

finish_upload_event(_Tag, undefined, _, _) -> 
    wf:flash("Por favor selecciona un archivo"),
    ok;

finish_upload_event(_Tag, _FileName, LocalFileName, _Node) ->
    ok = file:rename(LocalFileName, archivo()),
    wf:redirect("/descarga").

archivo() ->
    Dir = simple_bridge_util:get_env('scratch_dir'),
    filename:join([Dir, ?ORIGINAL]).
