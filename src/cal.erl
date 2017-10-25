%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(cal).

%% API exports
-export([calen_convert/1, calen_get_years/1, year_to_ets/1,
        month_to_weeks/2, ets_to_pdf/1]).

-include("records.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% @doc coge el ets del calendario y convierte las Notas 
%% para conseguir que reflejen el ciclo desde el "inicio" 
%% hasta el "fin"
-spec ets_to_pdf(Dbase :: ets:tid()) -> ets:tid().
ets_to_pdf(Dbase) ->
    Fun_foldl = fun(Reg=#calen{}, Tipo) when Tipo == nomatch andalso
                                            Reg#calen.nota == <<>> ->
                        nomatch;
                   (Reg=#calen{}, _Tipo) when Reg#calen.nota == <<"inicio">> ->
                        ets:insert(Dbase, Reg#calen{nota= <<"menstrua">>}),
                        menstrua;
                   (Reg=#calen{}, Tipo) when Tipo == menstrua andalso
                                              Reg#calen.nota == <<>> ->
                        ets:insert(Dbase, Reg#calen{nota= <<"menstrua">>}),
                        menstrua;
                   (Reg=#calen{}, _Tipo) when Reg#calen.nota == <<"fin">> ->
                        ets:insert(Dbase, Reg#calen{nota= <<>>}),
                        nomatch;
                   (Reg=#calen{}, _Tipo) when Reg#calen.nota == <<"pastilla">> ->
                        nomatch
    end,
    ets:foldl(Fun_foldl, nomatch, Dbase),
    Dbase.

% @doc nombre del mes
%
%% -spec name_of_month(Day, Lang) -> [char()] when
%%     Day  :: byte(),
%%     Lang :: {list()}.
%% name_of_month(Day,Lang) ->
%%     element(Day,Lang).

% @doc nombre del día de la semana.
%
%% -spec name_of_day(Day, Month, Year, Lang) -> [char()] when
%%     Day  :: byte(),
%%     Month:: byte(),
%%     Year :: integer(),
%%     Lang :: {list()}.
%% name_of_day(Day,Month,Year,Lang) ->
%%     element(calendar:day_of_the_week(Year,Month,Day),Lang).

% @doc Lista con todos los días del mes.
%
-spec month_all_days(Month,Year) -> [char()] when
    Month  :: byte(),
    Year   :: integer().
month_all_days(Month, Year) ->
    lists:seq(1,calendar:last_day_of_the_month(Year,Month)).

% @doc mapa con todos los meses del año, el key es el nombre del mes y el value es
% una lista con todos los días de ese mes.
%
%% -spec year_all_month(Year,Lang) -> map() when
%%     Year :: integer(),
%%     Lang:: {list()}.
%% year_all_month(Year, Lang) ->
%%     Result = maps:new(),
%%     year_all_month(Result, 12, Year, Lang).

%% -spec year_all_month(Result, Count, Year, Lang) -> Result when
%%     Result :: map(),
%%     Count  :: byte(),
%%     Year   :: integer(),
%%     Lang   :: {list()}.
%% year_all_month(Result, 0, _Year, _Lang) ->
%%     Result;
%% year_all_month(Result, Count, Year, Lang) ->
%%     Nombre = name_of_month(Count,Lang),
%%     Dias = month_all_days(Count,Year),
%%     NuevoResult = maps:put(Nombre,Dias,Result),
%%     year_all_month(NuevoResult, Count -1, Year, Lang).

% @doc Devuelve una lista de listas con todos los días del mes separados por
% semanas
%
-spec month_to_weeks(Month,Year) -> [[byte()]] when
    Month :: byte() | list(),
    Year  :: integer() | list().
month_to_weeks(Month, Year) when is_integer(Month), is_integer(Year)->
    Mes = month_all_days(Month, Year),
    DayOfWeek = calendar:day_of_the_week(Year,Month,1),
    Zero = lists:duplicate(DayOfWeek - 1, 0),
    {H,T} = lists:split(8-DayOfWeek,Mes),
    First = lists:append(Zero,H),
    month_to_weeks(T, [First]);
month_to_weeks([], Acc) ->
    lists:reverse(Acc);
month_to_weeks(List, Acc) when length(List) < 7 ->
    Zero = lists:duplicate(7 - length(List), 0),
    H = lists:append(List,Zero),
    month_to_weeks([],[H | Acc]);
month_to_weeks(List, Acc) when is_list(Acc), is_list(List) ->
    {H,T} = lists:split(7,List),
    month_to_weeks(T, [H | Acc]).

% @doc coge la lista de todos las lineas del archivo
% y lo convierte en una lista con los registros
% preparados para poder usarse en ets 
% {<<"20170904">>,{2017,9,4},<<"inicio">>}
-spec calen_convert(Data :: iodata()) -> [#calen{}].
calen_convert(Data) ->
    To_tuplas = [calen_split(Linea) || Linea <- Data],
    [calen_with_id(Tupla) || Tupla <- To_tuplas, Tupla =/= nomatch ].

% @doc Coge una tupla {{2017,9,4},<<"inicio">>} y lo convierte en 
% {<<"20170904">>,{2017,9,4},<<"inicio">>}
calen_with_id({Fecha, Nota}) ->
    #calen{id = date_to_binary(Fecha, Nota), fecha = Fecha, nota = Nota}.

% @doc devuelve una tupla con la fecha en una tupla y un string indicando el tipo de
% día -> inicio | final | pastilla | nomatch
-spec calen_split(Datos) -> {#date{}, binary()} when 
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
    Comando = fun ([]) -> <<"">>;
		          (Str) -> list_to_binary(string:lowercase(Str))
	      end,
    Fun = fun (nomatch) -> nomatch;
	          ({match, Result}) ->
                  Nota = Comando(lists:nth(4, Result)),
                  Date = #date{year=Anno(Result), month=Mes(Result), day=Dia(Result)},
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
% #date
%% -spec next_day(Fecha) -> #date{} when
%%         Fecha :: #date{}.
%% next_day(#date{year=Year, month=Month, day=Day}) ->
%%     {Y, M, D} =
%%         calendar:gregorian_days_to_date(
%%             calendar:date_to_gregorian_days({Year,Month,Day}) + 1),
%%     #date{year=Y,month=M,day=D}.

% @doc sacar de la Lista los años.
%
-spec calen_get_years(Lista) -> [calendar:year()] when
    Lista :: [#calen{}].
calen_get_years(Lista) ->
    Acc = lists:foldl(fun(Item, Annos) ->
                %% #calen{id=_I,#date{year=Year,month=_M,day=_D},nota=_N} = Item,
                #calen{id=_,fecha=#date{year=Year,month=_,day=_},nota=_} = Item,
                [Year|Annos]
            end,[],Lista),
    lists:usort(Acc).

year_to_ets(Year) ->
    Year_to_dates = fun(Lista, Y) ->
        {R,_} = lists:mapfoldl(fun(Item, Idx)->
                {lists:map(fun(D)-> #date{year=Y,month=Idx,day=D} end, Item), Idx +1}
             end,1,Lista),
        R
    end,
    Dates = lists:append(Year_to_dates(year_all_days(Year), Year)),
    Todos = [#calen{id=date_to_binary(X, <<"">>),fecha=X, nota= <<"">>} || X <- Dates],
    Todos.

% @doc todos los días de un año agrupados por meses
%
-spec year_all_days(Year) -> [list()] when
    Year :: calendar:year().
year_all_days(Year) ->  year_all_days(Year, 1, []).
year_all_days(_Year, 13, Acc) -> lists:reverse(Acc);
year_all_days(Year, Month, Acc) ->
    Mes = month_all_days(Month, Year),
    year_all_days(Year, Month + 1, [Mes|Acc]).

% @doc convierte la primera 3-tupla de tipo date() en binary
% para poder usarlo de clave primaria en ets y así poder
% ordenar por fecha
-spec date_to_binary(Tuple, Nota) -> binary() when
    Tuple :: #date{},
    Nota :: binary().
date_to_binary(#date{year=Year,month=Month,day=Day}, Nota) ->
    Corrector = fun(Num) when Num < 10 ->
                        "0" ++ integer_to_list(Num);
                   (Num) when Num >= 10 ->
                        integer_to_list(Num) 
                end,
    Pastilla = fun(Id, Not) when Not == <<"pastilla">> ->
                       Id ++ "X";
                  (Id, _Not) ->
                       Id
               end,
    Corregido = [Corrector(X) || X <- [Year, Month, Day]],
    list_to_binary(Pastilla(lists:join("", Corregido), Nota)).

%%============================================================
%% Tests
%% ===========================================================

-ifdef(TEST).

%% next_day_test() ->
%%     ?assertEqual(#date{year=2017,month=10,day=3},next_day(#date{year=2017,month=10,day=2})),
%%     ?assertEqual(#date{year=2017,month=11,day=1},next_day(#date{year=2017,month=10,day=31})),
%%     ?assertEqual(#date{year=2018,month=1,day=1},next_day(#date{year=2017,month=12,day=31})).
%%
%% calen_to_ets_test()->
%%     ?assertEqual(#calen{id = <<"20140903">>,
%%                         fecha = #date{year=2014,month=9,day=3}, 
%%                         nota = <<"inicio">>},
%%                  calen_to_ets({#date{year=2014,month=9,day=3},<<"inicio">>})).
%%
%% date_to_binary_test() ->
%%     ?assertEqual(<<"20171031">>, date_to_binary(#date{year=2017,month=10,day=31})),
%%     ?assertEqual(<<"20141101">>, date_to_binary(#date{year=2014,month=11,day=1})).
%%
%% calen_split_test() ->
%%     ?assertEqual({#date{year=2014,month=9,day=3}, <<"inicio">>},calen_split(<<"3 sept. 2014	Inicio del periodo"/utf8>>)),
%%     ?assertEqual({#date{year=2014,month=9,day=27},<<"fin">>},calen_split(<<"27 sept. 2014	Fin del periodo"/utf8>>)),
%%     ?assertEqual({#date{year=2017,month=4,day=27},nomatch},calen_split(<<"27 abr. 2017	Notas:Tttyuy"/utf8>>)),
%%     ?assertEqual({#date{year=2017,month=6,day=21},<<"pastilla">>},calen_split(<<"21 jun. 2017	Pastilla:qlaira,"/utf8>>)).
%%
%%
%% month_all_days_test() ->
%%     ?assertEqual(month_all_days(9,2017),[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30]).
%%
%% name_of_day_test() ->
%%     ?assertEqual(name_of_day(21,9,2016,?WEEK_NAME), 'miércoles'),
%%     ?assertEqual(name_of_day(1,9,2017,?WEEK_NAME), 'viernes').
%%
%% name_of_month_test() ->
%%     ?assertEqual(name_of_month(10,?MONTH_NAME), 'octubre'),
%%     ?assertEqual(name_of_month(1,?MONTH_NAME), 'enero').
%%
%% month_to_weeks_test() ->
%%     ?assertEqual(month_to_weeks(9,2017),
%%                 [[0,0,0,0,1,2,3],
%%                  [4,5,6,7,8,9,10],
%%                  [11,12,13,14,15,16,17],
%%                  [18,19,20,21,22,23,24],
%%                  [25,26,27,28,29,30,0]]).
-endif.
