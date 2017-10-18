%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (descarga).
-export([main/0]).
-include_lib("nitrogen_core/include/wf.hrl").
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
    %% DBase = crear_dbase(Fechas).
    error_logger:info_msg("->> ~p~n",[Fechas]),
    #panel{text = io_lib:format("->> ~p~n",[Fechas])}.


%% =================================================
%% ====== API INTERNO ==============================
%% =================================================

leer_archivo(Archivo) ->
    {ok, Datos} = file:read_file(Archivo),
    Lista = binary:split(Datos, [<<"\n">>], [global]),
    calen_convert(Lista).

% @doc coge la lista de todos las lineas del archivo
% y lo convierte en una lista con los registros
% preparados para poder usarse en ets 
% {<<"20170904">>,{2017,9,4},"inicio"}
calen_convert(Data) ->
    To_tuplas = [calen_split(Linea) || Linea <- Data],
    [calen_with_id(Tupla) || Tupla <- To_tuplas, Tupla =/= nomatch ].

% @doc Coge una tupla {{2017,9,4},"inicio"} y lo convierte en 
% {<<"20170904">>,{2017,9,4},"inicio"}
calen_with_id({Fecha, Nota}) ->
    #calen{id = tuple_to_binary(Fecha), fecha = Fecha, nota = Nota}.

% @doc convierte la primera 3-tupla de tipo date() en binary
% para poder usarlo de clave primaria en ets y así poder
% ordenar por fecha
-spec tuple_to_binary(Tuple) -> binary() when
    Tuple :: {integer(),byte(),byte()}.
tuple_to_binary(#fecha{year=Year,month=Month,day=Day}) ->
    Corrector = fun(Num) when Num < 10 ->
                        "0" ++ erlang:integer_to_list(Num);
                   (Num) when Num >= 10 ->
                        erlang:integer_to_list(Num) end,
    Corregido = [Corrector(X) || X <- [Year, Month, Day]],
    erlang:list_to_binary(Corregido).

% @doc devuelve una tupla con la fecha en una tupla y un string indicando el tipo de
% día -> inicio | final | pastilla | nomatch
-spec calen_split(Datos) -> {#fecha{}, binary()} when 
      Datos :: iodata().

calen_split(Datos) ->
    Int = fun (Str) ->
		  {Numero, _Resto} = string:to_integer(Str), Numero
	  end,
    Mes = fun (Str) -> convert_mes(lists:nth(2, Str)) end,
    Dia = fun (Str) -> Int(lists:nth(1, Str)) end,
    Anno = fun (Str) -> Int(lists:nth(3, Str)) end,
    Extraer = fun (Str, Pattern) ->
		      re:run(Str, Pattern,
			     [dotall, {capture, all_names, list}])
	      end,
    Comando = fun ([]) -> nomatch;
		  (Str) -> erlang:list_to_binary(string:lowercase(Str))
	      end,
    Fun = fun (nomatch) -> nomatch;
	      ({match, Result}) ->
		  Nota = Comando(lists:nth(4, Result)),
		  Date = #fecha{year=Anno(Result), month=Mes(Result), day=Dia(Result)},
          {Date, Nota}
	  end,
    Pattern =
	<<"^(?<A_dia>\\d{1,2})\\s*(?<B_mes>\\w*\\.).*(?<"
	  "C_ano>\\d{4})\\s+(?<D_comando>Inicio|Fin|Past"
	  "illa|).*$">>,
    Fun(Extraer(Datos, Pattern)).

% @doc convierte el mes en string a su correspondiente número del mes
% es para uso interno de calen_split
-spec convert_mes(Str) -> byte() when
    Str :: list().
convert_mes("ene.") -> 1;
convert_mes("feb.") -> 2;
convert_mes("mar.") -> 3;
convert_mes("abr.") -> 4;
convert_mes("may.") -> 5;
convert_mes("jun.") -> 6;
convert_mes("jul.") -> 7;
convert_mes("ago.") -> 8;
convert_mes("sept.") -> 9;
convert_mes("oct.") -> 10;
convert_mes("nov.") -> 11;
convert_mes("dic.") -> 12.

% @doc devuelve el siguiente día a uno dado en formato
% #fecha
-spec next_day(Fecha) -> #fecha{} when
        Fecha :: #fecha{}.
next_day(#fecha{year=Year, month=Month, day=Day}) ->
    {Y, M, D} =
        calendar:gregorian_days_to_date(
            calendar:date_to_gregorian_days({Year,Month,Day}) + 1),
    #fecha{year=Y,month=M,day=D}.
