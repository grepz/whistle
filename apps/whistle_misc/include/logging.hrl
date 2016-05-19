-ifndef(whistle_misc_loging_hrl).
-define(whistle_misc_logging_hrl, true).

-define(log(Lvl, Msg), whistle_misc:log(Lvl, Msg)).

-define(debug(Msg), ?log(debug, Msg)).
-define(error(Msg), ?log(error, Msg)).
-define(info(Msg), ?log(info, Msg)).

-endif. % whistle_misc_logging_hrl
