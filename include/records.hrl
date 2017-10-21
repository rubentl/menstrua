%% Include the automatically generated plugins directory
-include("plugins.hrl").

%% Include any application-specific custom elements, actions, or validators below

%% Nombre del archivo para guardar los datos subidos
-define (ORIGINAL, "datos.txt").
%% Nombre del archivo para descargar con el calendario
-define (NUEVO, "calendario.pdf").
%% Clave de acceso al programa
-define (CLAVE, <<214,105,29,52,104,226,246,106,49,145,158,64,173,2,87,86,210,60,238,117>>). 

-record(date, {year::calendar:year(),month::calendar:month(),day::calendar:day()}).
-record(calen, {id::binary(), fecha=#date{}, nota::binary()}).
