%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (clave).
-export([main/0, title/0, body/0, event/1]).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "No tengo una clave".

body() -> 
    [
    #panel{class="container", body=[
        #h1{class="titulo center-align", text="No tengo una clave"},
        #panel{class="card-panel", body=[
            #p{class="center-align", text="Si quieres tener acceso a la aplicación necesitarás una clave."},
            #p{class="center-align", body=["Para ello deberás ponerte en contacto conmigo en: ",                                      
                #link{text="polipo86@gmail.com", url="mailto:polipo86@gmail.com"}]},
            #panel{class="row", body=[
                #panel{class="col s2 offset-s5", body=[
                    #button{class="btn waves-effect waves-light center", text="Volver",
                            postback=volver}]
                }]
            }]
        }]
    }
    ].

event(volver) ->
    wf:redirect("/").
