-module(cal).

%% API exports
-export([name_of_month/2,name_of_day/4,
    month_all_days/2,year_all_month/2,month_to_weeks/2]).
%% -compile(export_all).

-include("cal.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% @doc nombre del mes
%
-spec name_of_month(Day, Lang) -> [char()] when
    Day  :: byte(),
    Lang :: {list()}.
name_of_month(Day,Lang) ->
    element(Day,Lang).

% @doc nombre del día de la semana.
%
-spec name_of_day(Day, Month, Year, Lang) -> [char()] when
    Day  :: byte(),
    Month:: byte(),
    Year :: integer(),
    Lang :: {list()}.
name_of_day(Day,Month,Year,Lang) ->
    element(calendar:day_of_the_week(Year,Month,Day),Lang).

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
-spec year_all_month(Year,Lang) -> map() when
    Year :: integer(),
    Lang:: {list()}.
year_all_month(Year, Lang) ->
    Result = maps:new(),
    year_all_month(Result, 12, Year, Lang).

-spec year_all_month(Result, Count, Year, Lang) -> Result when
    Result :: map(),
    Count  :: byte(),
    Year   :: integer(),
    Lang   :: {list()}.
year_all_month(Result, 0, _Year, _Lang) ->
    Result;
year_all_month(Result, Count, Year, Lang) ->
    Nombre = name_of_month(Count,Lang),
    Dias = month_all_days(Count,Year),
    NuevoResult = maps:put(Nombre,Dias,Result),
    year_all_month(NuevoResult, Count -1, Year, Lang).


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



%%============================================================
%% Tests
%% ===========================================================

-ifdef(TEST).

month_all_days_test() ->
    ?assertEqual(month_all_days(9,2017),[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30]).

name_of_day_test() ->
    ?assertEqual(name_of_day(21,9,2016,?WEEK_NAME), 'miércoles'),
    ?assertEqual(name_of_day(1,9,2017,?WEEK_NAME), 'viernes').

name_of_month_test() ->
    ?assertEqual(name_of_month(10,?MONTH_NAME), 'octubre'),
    ?assertEqual(name_of_month(1,?MONTH_NAME), 'enero').

month_to_weeks_test() ->
    ?assertEqual(month_to_weeks(9,2017),
                [[0,0,0,0,1,2,3],
                 [4,5,6,7,8,9,10],
                 [11,12,13,14,15,16,17],
                 [18,19,20,21,22,23,24],
                 [25,26,27,28,29,30,0]]).
-endif.
