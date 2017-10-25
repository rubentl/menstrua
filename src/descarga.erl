%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (descarga).
-export([main/0]).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() ->
    body(wf:user()).

body(auth) ->
    wf:content_type("application/pdf"),
    wf:download_as(?NUEVO),
    wf:continue(new, fun() -> wf:redirect("/") end, 3000),
    elPdf();

body(undefined) -> 
    wf:redirect("/").



elPdf()->
    % Nombre del archivo con el calendario subido
    Archivo = upload:archivo(),
    % Fechas del archivo en formato {{2014,8,12},"inicio"}
    Fechas = leer_archivo(Archivo),
    % Base de datos ETS con las Fechas 
    Dbase = crear_dbase(Fechas),
    elpdf:prueba(Dbase).


%% =================================================
%% ====== API INTERNO ==============================
%% =================================================


%% @doc A partir de la lista de tuplas obtenidas con el procesamiento
%% del archivo proporcionado por el usuario se crea la base de datos 
%% ets que permitirá generar el calendario en pdf
-spec crear_dbase(Fechas :: [tuple()] ) -> ets:tid().
crear_dbase(Fechas) ->
    Dbase = ets:new(dbase, [ordered_set, {keypos, #calen.id}]),
    Annos = cal:calen_get_years(Fechas),
    Fun_foreach = fun(Item) ->
                    ets:insert(Dbase, cal:year_to_ets(Item))
                  end,
    lists:foreach(Fun_foreach, Annos),
    ets:insert(Dbase,Fechas),
    cal:ets_to_pdf(Dbase).

leer_archivo(Archivo) ->
    {ok, Datos} = file:read_file(Archivo),
    file:delete(Archivo),
    Lista = binary:split(Datos, [<<"\n">>], [global]),
    cal:calen_convert(Lista).

