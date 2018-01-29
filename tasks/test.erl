-module(sample).
-export([xyz/1, p/0, g/0]).

xyz(Name) -> ?MODULE:Name().

p() -> "you called p".
g() -> "you called g".
