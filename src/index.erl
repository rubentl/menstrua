%% -*- mode: nitrogen -*-
-module (index).
-export([main/0, title/0, body/0, event/1, event_invalid/1]).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> 
    wf:clear_user(),
    #template { file="./site/templates/bare.html" }.

title() -> "Generador de calendario menstrual".

body() ->
    [
        #h1{text="Generador de calendario menstrual", class="titulo center-align"},
        #flash{},
        login()
    ].

login() ->
    wf:wire(login, #slide_down { speed=800 }),
    wf:wire(validar, password, #validate { validators=[
        #is_required { text="No puede quedar vacía la clave" },
        #custom {text="Inténtalo de nuevo.", tag=pass, function=fun passCorrecto/2 }
    ]}),
    [
        #panel{id=login, class="main", body=[
            #panel{id=formulario, class="formulario center-align", body=[
                #h2{text="Identifícate", class="subtitulo center-align"},
                #textbox{id=password, placeholder="Introduce la clave"}
            ]},
            #panel{class="botones center-align", body=[
                #link{id=validar, text="Entrar", class="izdo", postback=continuar},
                #link{text="No tengo clave", class="dcho", url="/clave"}
            ]},
            #panel{class="botones center-align", body=[
                #h5{text="Sólo quiero un calendario anual, gracias.",
                    class="center-align"},
                #textbox{id=anno, placeholder="Año", class="izdo"},
                #link{text="Calendario", class="dcho", postback=calendario}
            ]}
        ]}

    ].
	
event_invalid(continue) ->
    wf:flash("Inténtalo de nuevo.").

event(continuar) ->
    wf:user(auth),
    wf:redirect("/upload");
event(calendario) ->
    Anno = wf:q(anno),
    wf:redirect(lists:join("=",["/descarga?ano",Anno]));
event(_) -> ok.

passCorrecto(_Tag, Valor) ->
    wf:console_log(?CLAVE),
    crypto:hash(sha, Valor) == ?CLAVE.

