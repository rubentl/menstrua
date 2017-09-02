%% -*- mode: nitrogen -*-
-module (index).
-export([main/0, title/0, body/0, event/1, event_invalid/1]).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Generador de calendario menstrual".

body() ->
    [
        #h1{text="Generador de calendario menstrual", class="titulo center-align"},
        #flash{},
        login()
    ].

login() ->
    wf:wire(validar, password, #validate { validators=[
        #is_required { text="No puede quedar vacía la clave" },
        #custom {text="Inténtalo de nuevo.", tag=pass, function=fun passCorrecto/2 }
    ]}),
    [
        #panel{id=login, class="main", body=[
            #panel{id=formulario, class="formulario center-align", body=[
                #h2{text="Identifícate", class="subtitulo center-align"},
                #password{id=password, placeholder="Introduce la clave"}
            ]},
            #panel{class="botones center-align", body=[
                #link{id=validar, text="Entrar", class="izdo", postback=continuar},
                #link{text="No tengo clave", class="dcho", url="/clave"}
            ]}
        ]}
    ].
	
event_invalid(continue) ->
    wf:flash("Inténtalo de nuevo.").

event(continuar) ->
    wf:user(auth),
    wf:redirect("/upload");

event(_) -> ok.

passCorrecto(_Tag, Valor) ->
    Valor == ?CLAVE.

