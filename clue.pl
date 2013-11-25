%Things Known:
%              Each Card is in one location (player or file)
%              One of each type is in the case file
%              6 Suspects, 6 Weapons, 9 Rooms
%              Your Hand of Cards:
%	              3 Cards for 6 Players
%	              3-4 Cards for 5 Players:
%	               3 Cards for 2 Players and 4 Cards for 3 Players
%	              4-5 Cards for 4 Players:
%		             4 Cards for 2 Players and 5 Cards for 2 Players
%		            6 Cards for 3 Players
%	             Players hold 5 Suspects, 5 Weapons, 8 Rooms
%	             How Many Cards each player has
%Things Learned:
%              A player cannot refute (Doesn't have 3 cards)
%		            assert(player, suspect, weapon, room)).
%              A player refutes yours (Shows you one card they have)
%               assert(hasCard(player, card)).
%              A player refutes anothers (Don't know which card)
%              A player makes a wrong accusation

%Clauses of the form cantRefute(Player, Suspect, Weapon, Room)
:-dynamic cantRefute/4.

%Clauses of the form cantHave(Player, Card)
%Created via cantRefute
:-dynamic cantHaveSuspect/2.
:-dynamic cantHaveWeapon/2.
:-dynamic cantHaveRoom/2.

%Clauses of the form hasCard(Player, Card)
:-dynamic hasCard/2.
:-dynamic hasCardRoom/2.
:-dynamic hasCardSuspect/2.
:-dynamic hasCardWeapon/2.

%List of Suspect Cards
suspect(plum).
suspect(mustard).
suspect(green).
suspect(peacock).
suspect(scarlet).
suspect(white).

%List of Weapon Cards
weapon(rope).
weapon(leadpipe).
weapon(knife).
weapon(wrench).
weapon(candlestick).
weapon(pistol).

%List of Room Cards
room(hall).
room(lounge).
room(diningroom).
room(kitchen).
room(ballroom).
room(conservatory).
room(billiardroom).
room(library).
room(study).

% List of Players, Will need to be changed according to number of
% players in the game
player(plum).
player(mustard).
player(green).
player(peacock).
player(scarlet).
player(white).

% If a player can't refute a guess assert he cant refute those cards
cantRefute(Player, Suspect, Weapon, Room) :-
	cantHaveSuspect(Player, Suspect),
	cantHaveWeapon(Player, Weapon),
	cantHaveRoom(Player, Room).

% If a player refutes your guess assert he has a card
% Can also be used to list all of a players cards
hasCard(Player, Card) :- hasCardWeapon(Player, Card), weapon(Card).
hasCard(Player, Card) :- hasCardSuspect(Player, Card), suspect(Card).
hasCard(Player, Card) :- hasCardRoom(Player, Card), room(Card).


% A guess is correct if the Suspect, Weapon and Room are correct. For
% now only works for 6 players in the game.
correctGuess(Suspect, Weapon, Room) :-
	correctSuspect(Suspect),
	correctWeapon(Weapon),
	correctRoom(Room).

%TODO
% The correct suspect is chosen if all players dont have them or
% all suspects other than one are revealed
correctSuspect(Suspect).

%TODO
% The correct weapon is chosen if all players dont have it or all
% weapons other than one are revealed
correctWeapon(Weapon).

%TODO
% The correct room is chosen if all players don't have it or all rooms
% other than one are revealed
correctRoom(Room).

% A player can't have a weapon if another player has that card
cantHaveWeapon(Player, Weapon) :-
	hasCardWeapon(Other, Weapon),
	suspect(Other),
	suspect(Player),
	Player \== Other.

% A player can't have a suspect if another player has that card
cantHaveSuspect(Player, Suspect) :-
	hasCardSuspect(Other, Suspect),
	suspect(Other),
	suspect(Player),
	Player \== Other.

% A player can't have a room if another player has that card
cantHaveRoom(Player, Room) :-
	hasCardRoom(Other, Room),
	suspect(Other),
	suspect(Player),
	Player \== Other.
