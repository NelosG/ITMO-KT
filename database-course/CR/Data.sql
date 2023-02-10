insert into teams (TeamId, TeamName) values
    (1     , 'Cheba Queens'),
    (2     , 'No name'),
    (3     , 'Quiet Enough'),
    (4     , 'Orange Tree'),
    (5     , 'Turn off'),
    (6     , 'Lights out');

insert into Contests(ContestId, ContestName) values
    (1        , 'NWQC'),
    (2        , 'Practice 1'),
    (3        , 'Practice 2');

insert into Problems (ContestId, Letter, ProblemName) values
    (1        , 'A'   , 'Activist'),
    (1        , 'B'   , 'Bike'),
    (1        , 'C'   , 'Correspondence'),
    (1        , 'D'   , 'Diagram'),
    (1        , 'E'   , 'Easy LL/SC'),
    (1        , 'F'   , 'Failed Trends'),
    (1        , 'G'   , 'Grandma Path'),
    (1        , 'H'   , 'Heroes of SQL'),
    (1        , 'I'   , 'Integer Squared'),
    (1        , 'J'   , 'Just Password'),
    (1        , 'K'   , 'Keys Lost'),
    (1        , 'L'   , 'Lone Permutation'),
    (1        , 'M'   , 'Mind Trick'),
    (1        , 'N'   , 'Nuclear Shop'),
    (2        , 'X'   , 'XOR'),
    (2        , 'Y'   , 'Yell'),
    (2        , 'Z'   , 'Zero'),
    (3        , 'Y'   , 'Yell'),
    (3        , 'Z'   , 'Zero');

insert into Sessions(SessionId, TeamId, ContestId, Start) values
    (1        , 1     , 1        , '14.11.2020'),
    (2        , 2     , 1        , '14.11.2020'),
    (3        , 3     , 2        , '10.11.2020'),
    (4        , 3     , 2        , '14.11.2020'),
    (5        , 4     , 1        , '14.11.2020'),
    (6        , 4     , 2        , '14.11.2020'),
    (7        , 4     , 3        , '14.11.2020'),
    (8        , 5     , 1        , '14.11.2020'),
    (9        , 5     , 2        , '14.11.2020'),
    (10       , 5     , 3        , '14.11.2020'),
    (11       , 6     , 3        , '14.11.2020'),
    (12       , 6     , 1        , '14.11.2020');

insert into Runs (RunId, SessionId, Letter, SubmitTime, Accepted) values
    (1    , 1        , 'B'   , 71        , false),
(    2    , 1        , 'B'   , 87        , false),
(    3    , 1        , 'B'   , 157       , false),
(    4    , 1        , 'B'   , 202       , false),
(    5    , 1        , 'C'   , 2         , true),
(    6    , 1        , 'E'   , 6         , false),
(    7    , 1        , 'E'   , 96        , false),
(    8    , 1        , 'E'   , 106       , false),
(    9    , 1        , 'F'   , 4         , false),
(    10   , 1        , 'F'   , 89        , false),
(    11   , 1        , 'F'   , 186       , false),
(    12   , 1        , 'F'   , 259       , false),
(    13   , 1        , 'F'   , 322       , true),
(    14   , 1        , 'G'   , 52        , false),
(    15   , 1        , 'G'   , 71        , false),
(    16   , 1        , 'G'   , 146       , true),
(    17   , 1        , 'K'   , 52        , false),
(    18   , 1        , 'L'   , 64        , false),
(    19   , 1        , 'M'   , 30        , false),
(    20   , 1        , 'N'   , 90        , false),
(    21   , 1        , 'N'   , 128       , false),
(    22   , 2        , 'A'   , 2         , false),
(    23   , 2        , 'A'   , 101       , false),
(    24   , 2        , 'C'   , 4         , false),
(    25   , 2        , 'C'   , 94        , false),
(    26   , 2        , 'F'   , 24        , false),
(    27   , 2        , 'G'   , 72        , false),
(    28   , 2        , 'G'   , 90        , false),
(    29   , 2        , 'G'   , 148       , false),
(    30   , 2        , 'H'   , 43        , true),
(    31   , 2        , 'I'   , 85        , false),
(    32   , 2        , 'I'   , 97        , false),
(    33   , 2        , 'K'   , 26        , false),
(    34   , 2        , 'M'   , 50        , true),
(    35   , 2        , 'N'   , 56        , false),
(    36   , 2        , 'N'   , 84        , false),
(    37   , 3        , 'X'   , 52        , false),
(    38   , 3        , 'X'   , 127       , true),
(    39   , 3        , 'Y'   , 55        , true),
(    40   , 3        , 'Z'   , 81        , false),
(    41   , 3        , 'Z'   , 144       , true),
(    42   , 4        , 'Y'   , 55        , true),
(    43   , 5        , 'A'   , 84        , false),
(    44   , 5        , 'A'   , 103       , false),
(    45   , 5        , 'A'   , 174       , true),
(    46   , 5        , 'C'   , 57        , false),
(    47   , 5        , 'C'   , 91        , true),
(    48   , 5        , 'E'   , 26        , false),
(    49   , 5        , 'F'   , 91        , false),
(    50   , 5        , 'F'   , 112       , false),
(    51   , 5        , 'F'   , 199       , false),
(    52   , 5        , 'F'   , 223       , false),
(    53   , 5        , 'F'   , 295       , true),
(    54   , 5        , 'H'   , 54        , false),
(    55   , 5        , 'H'   , 114       , true),
(    56   , 5        , 'J'   , 72        , false),
(    57   , 5        , 'L'   , 73        , false),
(    58   , 5        , 'L'   , 126       , false),
(    59   , 5        , 'L'   , 221       , false),
(    60   , 5        , 'M'   , 98        , true),
(    61   , 5        , 'N'   , 15        , false),
(    62   , 5        , 'N'   , 52        , false),
(    63   , 5        , 'N'   , 139       , false),
(    64   , 5        , 'N'   , 227       , false),
(    65   , 5        , 'N'   , 264       , false),
(    66   , 5        , 'N'   , 329       , false),
(    67   , 5        , 'N'   , 400       , false),
(    68   , 6        , 'X'   , 72        , true),
(    69   , 6        , 'Y'   , 27        , true),
(    70   , 6        , 'Z'   , 99        , false),
(    71   , 6        , 'Z'   , 103       , true),
(    72   , 7        , 'Y'   , 18        , true),
(    73   , 7        , 'Z'   , 17        , false),
(    74   , 8        , 'B'   , 1         , false),
(    75   , 8        , 'B'   , 83        , false),
(    76   , 8        , 'B'   , 94        , false),
(    77   , 8        , 'B'   , 119       , false),
(    78   , 8        , 'C'   , 47        , false),
(    79   , 8        , 'C'   , 58        , false),
(    80   , 8        , 'D'   , 47        , false),
(    81   , 8        , 'D'   , 100       , false),
(    82   , 8        , 'D'   , 108       , false),
(    83   , 8        , 'E'   , 28        , false),
(    84   , 8        , 'G'   , 88        , true),
(    85   , 8        , 'H'   , 24        , false),
(    86   , 8        , 'J'   , 12        , true),
(    87   , 8        , 'K'   , 68        , false),
(    88   , 8        , 'L'   , 76        , true),
(    89   , 9        , 'X'   , 19        , true),
(    90   , 9        , 'Y'   , 74        , false),
(    91   , 10       , 'Y'   , 72        , false),
(    92   , 10       , 'Y'   , 141       , false),
(    93   , 10       , 'Y'   , 224       , false),
(    94   , 10       , 'Y'   , 252       , true),
(    95   , 10       , 'Z'   , 72        , false),
(    96   , 10       , 'Z'   , 139       , false),
(    97   , 10       , 'Z'   , 168       , false),
(    98   , 10       , 'Z'   , 187       , false),
(    99   , 10       , 'Z'   , 286       , true),
(    100  , 10       , 'Z'   , 300       , true),
(    101  , 10       , 'Z'   , 394       , false),
(    102  , 11       , 'Z'   , 394       , false);