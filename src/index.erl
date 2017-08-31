%% -*- mode: nitrogen -*-
-module (index).
-export([main/0, title/0, body/0, event/1]).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Generador de calendario menstrual".

body() ->
    [
        #h1{text="Generador de calendario menstrual", class="titulo center-align"},
        login()
    ].

login() -> 
    [
        #panel{id=login, class="row", body=[
            #panel{id=formulario, class="col s12 m6 offset-m3", body=[
                #panel{class="card", body=[
                    #panel{class="card-content", body=[
                        #span{class="card-title center-align",text="Identifícate"},
                        #panel{class="row", body=[
                            #panel{id=pass, class="col s12 input-field", body=[
                                #password{class="validate", id=password, 
                                          placeholder="Introduce la clave",
                                          postback=entrar}]
                            }]
                        }]
                    },
                    #panel{class="card-action", body=[
                        #link{text="Entrar", postback=entrar},
                        #link{text="No tengo clave", url="/clave"}]
                    }]
                }]
            }]
        }
    ].
	
event(entrar) ->
    case wf:q(password) == "puy" of 
        true -> wf:redirect("/upload");
        false -> wf:insert_bottom(pass,archivo(mal))
    end.

archivo(mal) ->
    #p{text="Inténtalo de nuevo", class="center-align red-text text-darken-1"}.

