%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (descarga).
-export([main/0]).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("records.hrl").

main() ->
    body(wf:user()).

body(auth) ->
    %% wf:content_type("application/pdf"),
    %% wf:download_as("calendario.pdf"),
    %% wf:content_type("text/plain"),
    %% wf:download_as("calendario.txt"),
    elPdf();

body(undefined) -> 
    wf:redirect("/").



elPdf()->
    % Nombre del archivo con el calendario subido
    Archivo = upload:archivo(),
    % Fechas del archivo en formato {{2014,8,12},"inicio"}
    Fechas = leer_archivo(Archivo),
    % Base de datos ETS con las Fechas 
    % TODO
    Dbase = crear_dbase(Fechas),

    wf:info("==> ~p ~n ",[ets:select(Dbase,
            ets:fun2ms(fun(X = #calen{id= I, nota=T})
                             when T == <<"pastilla">> orelse
                                  I == <<"20161231">> -> X end))
                         ]).


%% =================================================
%% ====== API INTERNO ==============================
%% =================================================

crear_dbase(Fechas) ->
    Dbase = ets:new(dbase, [ordered_set, {keypos, #calen.id}]),
    % TODO
    Annos = cal:calen_get_years(Fechas),
    lists:foreach(fun(Item)->ets:insert(Dbase, cal:year_to_ets(Item)) end, Annos),
    ets:insert(Dbase,Fechas),
    Dbase.

leer_archivo(Archivo) ->
    {ok, Datos} = file:read_file(Archivo),
    Lista = binary:split(Datos, [<<"\n">>], [global]),
    cal:calen_convert(Lista).

