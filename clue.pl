%Things Known at Start:
%	      Each Card is in one location (player or file)
%	      One of each type is in the case file
%	      6 Suspects, 6 Weapons, 9 Rooms
%	      Your Hand of Cards:
%		 3 Cards for 6 Players
%		 3-4 Cards for 5 Players:
%		   3 Cards for 2 Players and 4 Cards for 3 Players
%		 4-5 Cards for 4 Players:
%	           4 Cards for 2 Players and 5 Cards for 2 Players
%	         6 Cards for 3 Players
%	      Players hold 5 Suspects, 5 Weapons, 8 Rooms
%	      How Many Cards each player has
%Things Learned in Game:
%	      A player cannot refute (Doesn't have 3 cards)
%	         assert(cantHaveSuspect(Player, Suspect)),
%	         assert(cantHaveWeapon(Player, Weapon)),
%	         assert(cantHaveRoom(Player, Room)).
%	      A player reveals a card
%	         assert(hasCardX(Player, Y)).
%	           Where X is either Suspect, Weapon, or Room.
%	           And Y is a card of that type.

%Dynamically add which players are in the game,
%Turn order should be based on order added,
%e.g. First player at top, Second second from top ect.
%Accomplish this by adding using assertz(player(X).
%in order of players turn.
:-dynamic player/1.

%Clauses of the form cantHave(Player, Card)
:-dynamic cantHaveSuspect/2.
:-dynamic cantHaveWeapon/2.
:-dynamic cantHaveRoom/2.

%Clauses of the form hasCard(Player, Card)
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

% Test if a player can't refute a guess
cantRefute(Player, Suspect, Weapon, Room) :-
	cantHaveSuspect(Player, Suspect),
	cantHaveWeapon(Player, Weapon),
	cantHaveRoom(Player, Room).


% Can be used to list all of a players cards, or test if a player has a
% certain card
hasCard(Player, Card) :-
	hasCardWeapon(Player, Card),
	weapon(Card),
	player(Player).
hasCard(Player, Card) :-
	hasCardSuspect(Player, Card),
	suspect(Card),
	player(Player).
hasCard(Player, Card) :-
	hasCardRoom(Player, Card),
	room(Card),
	player(Player).


% A guess is correct if the Suspect, Weapon and Room are correct. For
% now only works for 6 players in the game.
correctGuess(Suspect, Weapon, Room) :-
	correctSuspect(Suspect),
	correctWeapon(Weapon),
	correctRoom(Room).

%TODO
% The correct suspect is chosen if all players dont have them or
% all suspects other than one are revealed
correctSuspect(Answer) :-
	(lastUnheldSuspect(Answer);
	allCantHaveSuspect(Answer)).

%TODO
% The correct weapon is chosen if all players dont have it or all
% weapons other than one are revealed
correctWeapon(Answer) :-
	(lastUnheldWeapon(Answer);
	allCantHaveWeapon(Answer)).

% The correct room is found if it is the last unheld card, or if all
% players cannot have that card.
correctRoom(Answer) :-
	(lastUnheldRoom(Answer);
	allCantHaveRoom(Answer)).

allCantHaveSuspect(Answer) :-
	findall(X, player(X), L1),
	allCantHaveSuspectH(L1, Answer).

allCantHaveSuspectH([], _).
allCantHaveSuspectH([H|T], Answer) :-
	cantHaveSuspect(H, Answer),
	allCantHaveSuspectH(T, Answer).

allCantHaveWeapon(Answer) :-
	findall(X, player(X), L1),
	allCantHaveWeaponH(L1, Answer).

allCantHaveWeaponH([], _).
allCantHaveWeaponH([H|T], Answer) :-
	cantHaveWeapon(H, Answer),
	allCantHaveWeaponH(T, Answer).

allCantHaveRoom(Answer) :-
	findall(X, player(X), L1),
	allCantHaveRoomH(L1, Answer).

allCantHaveRoomH([], _).
allCantHaveRoomH([H|T], Answer) :-
	cantHaveRoom(H, Answer),
	allCantHaveRoomH(T, Answer).

%First way to solve, 5/6 suspects known to be held.
lastUnheldSuspect(Answer) :-
	remainingSuspects(Remaining),
	length(Remaining, 1),
	nth0(0, Remaining, Answer).

%First way to solve, 5/6 weapons known to be held.
lastUnheldWeapon(Answer) :-
	remainingWeapons(Remaining),
	length(Remaining, 1),
	nth0(0, Remaining, Answer).

%First way to solve, 8/9 rooms known to be held.
lastUnheldRoom(Answer) :-
	remainingRooms(Remaining),
	length(Remaining, 1),
	nth0(0, Remaining, Answer).

%Returns all suspects whose location isn't known.
remainingSuspects(Suspects) :-
	findall(X, hasCardSuspect(_,X), L1),
	findall(Y, suspect(Y), L2),
	subtract(L2, L1, Suspects).

%Returns all weapons thats location isn't known.
remainingWeapons(Weapons) :-
	findall(X, hasCardWeapon(_,X), L1),
	findall(Y, weapon(Y), L2),
	subtract(L2, L1, Weapons).

%Returns all rooms thats locations isn't known.
remainingRooms(Rooms) :-
	findall(X, hasCardRoom(_,X), L1),
	findall(Y, room(Y), L2),
	subtract(L2, L1, Rooms).


% A player can't have a weapon if another player has that card
cantHaveWeapon(Player, Weapon) :-
	hasCardWeapon(Other, Weapon),
	player(Other),
	player(Player),
	Player \== Other.

% A player can't have a suspect if another player has that card
cantHaveSuspect(Player, Suspect) :-
	hasCardSuspect(Other, Suspect),
	player(Other),
	player(Player),
	Player \== Other.

% A player can't have a room if another player has that card
cantHaveRoom(Player, Room) :-
	hasCardRoom(Other, Room),
	player(Other),
	player(Player),
	Player \== Other.
