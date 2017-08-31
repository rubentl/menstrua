%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (upload).
-export([main/0, title/0, body/0, event/1, finish_upload_event/4,
         start_upload_event/1]).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Carga el archivo con las fechas".

body() -> 
    [
        #h1{text="Archivo con las fechas", class="titulo center-align"},
        fechas()
    ].

fechas() -> 
    [
    #panel{class="container", body=[
        #panel{id=panel, class="card-panel", body=[
            #panel{class="row", body=[
                #panel{id=archivo, class="col s10 offset-s1 m4 offset-m4", body=[
                    #upload{
                        tag=archivo, file_text="Selecciona el archivo",
                        show_button=false
                    }]
                }]
            },
            #hr{},
            #panel{class="row", body=[
                #panel{class="col s2 offset-s4", body=[
                    #button{class="btn waves-effect waves-light",
                            text="Cancelar", postback=cancelar}]
                }]
            }]
        }]
    }
    ].
	
event(cancelar) ->
    wf:redirect("/").

start_upload_event(_) ->
    ok.

finish_upload_event(_Tag, undefined, _, _) -> 
    wf:insert_bottom(archivo,archivo(mal)),
    ok;

finish_upload_event(Tag, FileName, LocalFileName, Node) ->
    FileSize = filelib:file_size(LocalFileName),
    wf:flash(wf:f("Uploaded file (~p): ~ts (~p bytes) to ~s on node ~s.", [Tag, FileName, FileSize, LocalFileName, Node])),
    wf:redirect("/descarga?archivo=" ++ LocalFileName).

archivo(mal) ->
    #p{text="Por favor selecciona un archivo", class="center-align red-text text-darken-1"}.
