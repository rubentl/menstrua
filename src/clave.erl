%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (clave).
-export([main/0, title/0, body/0, event/1]).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "No tengo una clave".

%% Información del modo implementado para la obtención de una clave que de acceso
%% a la aplicación
%%
body() -> 
    [
        #h1{text="No tengo una clave", class="titulo center-align"},
        #flash{},
        #panel{class="main", body=[
            #p{class="center-align", text="Si quieres tener acceso a la aplicación
               necesitarás una clave."
            },
            #p{class="center-align", body=["Para ello deberás ponerte en contacto conmigo en: ",                                      
                #link{text="polipo86@gmail.com", url="mailto:polipo86@gmail.com"}
            ]},
            #panel{class="botones center-align", body=[
                #link{text="Volver", postback=volver}
            ]}
        ]}
    ].

%% La única opción posible es volver a la página de inico
%%
event(volver) ->
    wf:redirect("/").
