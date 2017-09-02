%% Include the automatically generated plugins directory
-include("plugins.hrl").

%% Include any application-specific custom elements, actions, or validators below

%% Nombre del archivo para guardar los datos subidos
-define (ORIGINAL, "datos.txt").
%% Nombre del archivo para descargar con el calendario
-define (NUEVO, "calendario.pdf").
-define (CLAVE, "puy"). %% Cambiar en producción
