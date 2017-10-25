-module(elpdf).

-export([prueba/1, 
        dbase_to_annos/1,
        ets_to_weeks/4]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("records.hrl").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

fuente() -> "Helvetica".
fuente_bold() -> "Helvetica-Bold".
tamano() -> 12.
tamano_large() -> 40.
menstrua_color() -> red.
pastilla_color() -> green.

prueba(Dbase) ->
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF,a4),
    eg_pdf:set_author(PDF,"Rubén Toca Lucio"),
    eg_pdf:set_title(PDF, "Calendario menstrual"),
    eg_pdf:set_date(PDF,2017,10,15),
    
    Annos = dbase_to_annos(Dbase),
    anno(PDF, Dbase, Annos),
    
    {Serialised, _PageNo} = eg_pdf:export(PDF),
    eg_pdf:delete(PDF),
    Serialised.

dbase_to_annos(Dbase) ->
    lists:usort(
        ets:select(Dbase,
            ets:fun2ms(fun(#calen{fecha=#date{year=Y}}) -> Y end)
        )
    ).

ets_to_weeks(Dbase, SemList, Mes, Anno) ->
    ets_to_weeks(Dbase, SemList, Mes, Anno, []).
ets_to_weeks(_Dbase, [], _Mes, _Anno, Acc) ->  lists:reverse(Acc);
ets_to_weeks(Dbase, [H|T], Mes, Anno, Acc) ->
    Fun = fun(D) when D /= 0 -> 
                Nota = nota(Dbase, Anno, Mes, D),
                {D, Nota};
             (D) when D == 0 ->
                {D, []}
          end,
    W = [Fun(Dia) || Dia <- H], 
    ets_to_weeks(Dbase, T, Mes, Anno, [W|Acc]).

nota(Dbase, Y, M, D) ->
    Rec = ets:select(Dbase,ets:fun2ms(
        fun(#calen{fecha=#date{year=Ye,month=Mo,day=Da},nota=Nota})
              when Ye == Y andalso Mo == M andalso Da == D
              -> Nota end)),
    [binary_to_atom(X, utf8) || X <- Rec, X /= <<>>].
    %% [X || X <- Rec].

anno(_PDF, _Dbase, []) -> ok;
anno(PDF, Dbase, [Anno|T]) ->
    titulo(PDF,Anno),
    leyendas(PDF),
    Fun_foreach = fun(Mes) -> 
                    mes_tabla(PDF, Dbase, Anno, Mes)
                 end,
    lists:foreach(Fun_foreach, lists:seq(1,12)),
    eg_pdf:new_page(PDF),
    anno(PDF, Dbase, T).

mes_tabla(PDF, Dbase, Anno, Mes) ->
    mes_cabecera(PDF, Mes),
    SemList = cal:month_to_weeks(Mes, Anno),
    Semanas = ets_to_weeks(Dbase, SemList, Mes, Anno),
    Fun_foldl = fun(Item, Acc) -> 
                    mes_semana(PDF, Mes, Acc, Item),
                    Acc + 1
                end,
    lists:foldl(Fun_foldl, 1, Semanas),
    ok.

mes_cabecera(PDF, Mes) ->
    NombreMes = lists:nth(Mes,mes_nombre()),
    {X, Y, Ancho, _Alto} = mes_xy(Mes),
    texto_centrar(PDF, fuente_bold(), tamano() + 2, X + (Ancho/2), Y, NombreMes),
    DiaX = Ancho / 7,
    fila(PDF, X + (DiaX / 2), Y - 20, DiaX, dia_nombre()),
    ok.

mes_semana(PDF, Mes, Semana, Dias) ->
    {X, Y, Ancho, _Alto} = mes_xy(Mes),
    AnchoDia = Ancho /7,
    Fila = Y - (Semana * 20),
    fila(PDF, X + (AnchoDia / 2), Fila - 20, AnchoDia, Dias),
    ok.


fila(_PDF, _X, _Y, _IncX, []) -> ok;
fila(PDF, X, Y, IncX, [H|T]) when is_tuple(H) ->
    eg_pdf:save_state(PDF),
    {Dia, Nota} = H,
    texto_centrar(PDF, X, Y, Dia),
    Fun_foreach = fun(Item) when Item == menstrua ->
                        eg_pdf:set_fill_color(PDF, menstrua_color()),
                        eg_pdf:rectangle(PDF, {X-(IncX/2), Y-3},{IncX, 2}),
                        eg_pdf:path(PDF, fill);
                    (Item) when Item == pastilla ->
                        eg_pdf:set_fill_color(PDF, pastilla_color()),
                        eg_pdf:rectangle(PDF, {X-(IncX/2), Y-7},{IncX, 2}),
                        eg_pdf:path(PDF, fill);
                    (Item) -> Item 
                  end,
    lists:foreach(Fun_foreach, Nota),
    eg_pdf:restore_state(PDF),
    fila(PDF, X + IncX, Y, IncX, T);
fila(PDF, X, Y, IncX, [H|T]) when not is_tuple(H) ->
    texto_centrar(PDF, X, Y, H),
    fila(PDF, X + IncX, Y, IncX, T).

mes_xy(Num) ->
    {_, _, AnchoPagina, AltoPagina} = eg_pdf:pagesize(a4),
    Alto = trunc((AltoPagina - 100) / 4),
    Fil = [{lists:member(Num, [1,2,3]),4},
           {lists:member(Num, [4,5,6]),3},
           {lists:member(Num, [7,8,9]),2},
           {lists:member(Num, [10,11,12]),1}],
    {true, Fila} = lists:keyfind(true,1,Fil),
    Ancho = trunc((AnchoPagina - 100) / 3),
    Col = [{lists:member(Num, [1,4,7,10]),0},
           {lists:member(Num, [2,5,8,11]),1},
           {lists:member(Num, [3,6,9,12]),2}],
    {true, Columna} = lists:keyfind(true,1,Col),
    {25 + (25*Columna) + (Ancho*Columna) , Alto*Fila, Ancho, Alto}.

titulo(PDF, Str) ->
    AnchoTexto = eg_pdf:get_string_width(PDF, fuente_bold(), tamano_large(), int_to_txt(Str)),
    {_, _, AnchoPagina, AltoPagina} = eg_pdf:pagesize(a4),
    X = (AnchoPagina - AnchoTexto) / 2,
    Y = AltoPagina - 50,
    texto(PDF, fuente_bold(), tamano_large(), X, Y, Str),
    ok.

leyendas(PDF) ->
    eg_pdf:save_state(PDF),
    {_, _, _AnchoPagina, AltoPagina} = eg_pdf:pagesize(a4),
    eg_pdf:set_fill_color(PDF, menstrua_color()),
    eg_pdf:rectangle(PDF, {25, AltoPagina - 50},{25,6}),
    eg_pdf:path(PDF, fill),
    texto(PDF, 55, AltoPagina - 50, "Menstruacion"),
    eg_pdf:set_fill_color(PDF, pastilla_color()),
    eg_pdf:rectangle(PDF, {25, AltoPagina - 35},{25,6}),
    eg_pdf:path(PDF, fill),
    texto(PDF, 55, AltoPagina - 35, "Pastilla"),
    eg_pdf:restore_state(PDF),
    ok.

int_to_txt(0) -> "";
int_to_txt(T) when is_integer(T) -> integer_to_list(T);
int_to_txt(T) when is_list(T) -> T.

texto(PDF, Fuente, Tamano, X, Y, Str) ->
    eg_pdf:set_font(PDF, Fuente, Tamano),
    eg_pdf:moveAndShow(PDF, X, Y, int_to_txt(Str)),
    ok.

texto(PDF, X, Y, Str) ->
    texto(PDF, fuente(), tamano(), X, Y, Str),
    ok.

texto_centrar(PDF, Fuente, Tamano, X, Y, Str) ->
    AnchoTexto = eg_pdf:get_string_width(PDF, Fuente, Tamano, int_to_txt(Str)),
    texto(PDF, Fuente, Tamano, X - (AnchoTexto/2), Y, Str),
    ok.

texto_centrar(PDF, X, Y, Str) ->
    texto_centrar(PDF, fuente(), tamano(), X, Y, Str),
    ok.
    
dia_nombre() -> ["L","M","X","J","V","S","D"].
mes_nombre() -> ["Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto",
    "Septiembre","Octubre","Noviembre","Diciembre"].



-ifdef(TEST).

prueba_test() ->
    ?assertEqual([], prueba()).

-endif.
