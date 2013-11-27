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
%
%Things Learned in Game:
%	      A player cannot refute (Doesn't have 3 cards)
%	         assert(cantHaveSuspect(Player, Suspect)),
%	         assert(cantHaveWeapon(Player, Weapon)),
%	         assert(cantHaveRoom(Player, Room)).
%	      A player reveals a card
%	         assert(hasCardX(Player, Y)).
%	           Where X is either Suspect, Weapon, or Room.
%	           And Y is a card of that type.

%Dynamically add which players are in the game, who's turn it is
%and which character is the player
:-dynamic player/1.
:-dynamic turn/1.
:-dynamic userIs/1.

%Clauses of the form cantHave(Player, Card)
:-dynamic cantHaveSuspect/2.
:-dynamic cantHaveWeapon/2.
:-dynamic cantHaveRoom/2.

%Clauses of the form hasCard(Player, Card)
:-dynamic hasCardSuspect/2.
:-dynamic hasCardWeapon/2.
:-dynamic hasCardRoom/2.

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

% Sets turn to whichever player goes first, scarlet if in game
% otherwise mustard, white ect
firstTurn :-
	player(X),
	assert(turn(X)).

% Changes turn/1 to the next player who is in the games turn.
nextPlayer :-
	turn(X),
	retract(turn(X)),
	nextPlayerH(X).

nextPlayerH(scarlet) :-
	(player(mustard),
	 assert(turn(mustard)),!);
	nextPlayerH(mustard).
nextPlayerH(mustard) :-
	(player(white),
	 assert(turn(white)),!);
	nextPlayerH(white).
nextPlayerH(white) :-
	(player(green),
	 assert(turn(green)),!);
	nextPlayerH(green).
nextPlayerH(green) :-
	(player(peacock),
	 assert(turn(peacock)),!);
	nextPlayerH(peacock).
nextPlayerH(peacock) :-
	(player(plum),
	 assert(turn(plum)),!);
	nextPlayerH(plum).
nextPlayerH(plum) :-
	(player(scarlet),
	 assert(turn(scarlet)),!);
	nextPlayerH(scarlet).



% Test if a player can't refute a guess
cantRefute(Player, Suspect, Weapon, Room) :-
	cantHaveSuspect(Player, Suspect),
	cantHaveWeapon(Player, Weapon),
	cantHaveRoom(Player, Room).

listCards(Player, X) :-
	(hasCardSuspect(Player, X);
	hasCardWeapon(Player, X);
	hasCardRoom(Player, X)).

% Used to assert the players starting cards
% Or new cards as they are discovered
hasCard(Player, Card) :-
	hasCardWeapon(Player, Card);
	(weapon(Card),
	player(Player),
	assert(hasCardWeapon(Player, Card))).

hasCard(Player, Card) :-
	hasCardSuspect(Player, Card);
	(suspect(Card),
	player(Player),
	assert(hasCardSuspect(Player, Card))).

hasCard(Player, Card) :-
	hasCardRoom(Player, Card);
	(room(Card),
	player(Player),
	assert(hasCardRoom(Player, Card))).

%Used in game to assert the cards the player doesn't hold
playerDoesntHave(Player) :-
	pDHWeapon(Player),
	pDHSuspect(Player),
	pDHRoom(Player).

pDHSuspect(Player) :-
	assert(cantHaveSuspect(Player, plum)),
	assert(cantHaveSuspect(Player, mustard)),
	assert(cantHaveSuspect(Player, green)),
	assert(cantHaveSuspect(Player, white)),
	assert(cantHaveSuspect(Player, scarlet)),
	assert(cantHaveSuspect(Player, peacock)),
	(hasCardSuspect(Player, X),
	retractall(cantHaveSuspect(Player, X)));!.

pDHWeapon(Player) :-
	assert(cantHaveWeapon(Player, rope)),
	assert(cantHaveWeapon(Player, leadpipe)),
	assert(cantHaveWeapon(Player, knife)),
	assert(cantHaveWeapon(Player, wrench)),
	assert(cantHaveWeapon(Player, candlestick)),
	assert(cantHaveWeapon(Player, pistol)),
	((hasCardWeapon(Player, X),
	retractall(cantHaveWeapon(Player, X)));!).

pDHRoom(Player) :-
	assert(cantHaveRoom(Player, hall)),
	assert(cantHaveRoom(Player, lounge)),
	assert(cantHaveRoom(Player, diningroom)),
	assert(cantHaveRoom(Player, kitchen)),
	assert(cantHaveRoom(Player, ballroom)),
	assert(cantHaveRoom(Player, conservatory)),
	assert(cantHaveRoom(Player, billiardroom)),
	assert(cantHaveRoom(Player, library)),
	assert(cantHaveRoom(Player, study)),
	((hasCardRoom(Player, X),
	retractall(cantHaveRoom(Player, X)));!).


% A guess is correct if the Suspect, Weapon and Room are correct, if
% the solution is found can be used to return it.
correctGuess(Suspect, Weapon, Room) :-
	correctSuspect(Suspect),
	correctWeapon(Weapon),
	correctRoom(Room).

% The correct suspect is found if it is the last unheld suspect, or if
% all players cannot have that suspect.
correctSuspect(Answer) :-
	(lastUnheldSuspect(Answer);
	allCantHaveSuspect(Answer)).

% The correct weapon is found if it is the last unheld weapon, or if all
% players cannot have that weapon.
correctWeapon(Answer) :-
	(lastUnheldWeapon(Answer);
	allCantHaveWeapon(Answer)).

% The correct room is found if it is the last unheld room, or if all
% players cannot have that room.
correctRoom(Answer) :-
	(lastUnheldRoom(Answer);
	allCantHaveRoom(Answer)).

%Finds a Suspect all Players can't have, false if there isn't one
allCantHaveSuspect(Answer) :-
	findall(X, player(X), L1),
	allCantHaveSuspectH(L1, Answer).

allCantHaveSuspectH([], _).
allCantHaveSuspectH([H|T], Answer) :-
	cantHaveSuspect(H, Answer),
	allCantHaveSuspectH(T, Answer).

%Finds a Weapon all players can't have, false if there isn't one
allCantHaveWeapon(Answer) :-
	findall(X, player(X), L1),
	allCantHaveWeaponH(L1, Answer).

allCantHaveWeaponH([], _).
allCantHaveWeaponH([H|T], Answer) :-
	cantHaveWeapon(H, Answer),
	allCantHaveWeaponH(T, Answer).

%Finds a Room all players can't have, false if there isn't one
allCantHaveRoom(Answer) :-
	findall(X, player(X), L1),
	allCantHaveRoomH(L1, Answer).

allCantHaveRoomH([], _).
allCantHaveRoomH([H|T], Answer) :-
	cantHaveRoom(H, Answer),
	allCantHaveRoomH(T, Answer).

% First way to solve, 5/6 suspects known to be held, or false if more
% than one suspect aren't known to be held
lastUnheldSuspect(Answer) :-
	remainingSuspects(Remaining),
	length(Remaining, 1),
	nth0(0, Remaining, Answer).

% First way to solve, 5/6 weapons known to be held, or false if more
% than one weapon isn't known to be held.
lastUnheldWeapon(Answer) :-
	remainingWeapons(Remaining),
	length(Remaining, 1),
	nth0(0, Remaining, Answer).

% First way to solve, 8/9 rooms known to be held, or false if more than
% one room isn't known to be held.
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

%UI Work Below Here:

start(Suspect, Weapon, Room) :-
	init,
	playerTurn,
	correctGuess(Suspect, Weapon, Room),
	end.

init :-
	write('How many players are in the game?\n'),
	read(NP),
	number(NP),
	generatePlayers(NP),
	firstTurn,
	write('Which character are you?\n'),
	read(YC),
	player(YC),
	assert(userIs(YC)),
	knownCards(YC).

end :-
	retractall(turn(_)),
	retractall(userIs(_)),
	retractall(player(_)),
	retractall(cantHaveSuspect(_,_)),
	retractall(cantHaveWeapon(_,_)),
	retractall(cantHaveRoom(_,_)),
	retractall(hasCardSuspect(_,_)),
	retractall(hasCardWeapon(_,_)),
	retractall(hasCardRoom(_,_)).

playerTurn :-
	write('It is '),
	turn(Y),
	write(Y),
	write('\'s turn. '),
	(userIs(Z),
	 Z == Y,
	 playerTurnH(y));
	playerTurnH(n).

playerTurnH(y) :-
	write('Did you make an accusation? (y/n)\n'),
	read(Y),
	accusationP(Y).

playerTurnH(n) :-
	write('Did they make an accusation? (y/n)\n'),
	read(Y),
	accusationO(Y).

accusationP(y) :-
	write('Who was the suspect? \n'),
	read(S),
	suspect(S),
	write('What was the weapon? \n'),
	read(W),
	weapon(W),
	write('In what room did it happen? \n'),
	read(R),
	room(R),
	write('How many players couldn\'t refute it? \n'),
	read(NPCR),
	accusationH(S, W, R, NPCR),
	write('Who refuted it? \n'),
	read(CO),
	player(CO),
	write('Which card did they have? \n'),
	read(CH),
	hasCard(CO, CH),
	finishTurn.

accusationP(n) :-
	finishTurn.

accusationO(y) :-
	write('Who was the suspect? \n'),
	read(S),
	suspect(S),
	write('What was the weapon? \n'),
	read(W),
	weapon(W),
	write('In what room did it happen? \n'),
	read(R),
	room(R),
	write('How many players couldn\'t refute it? \n'),
	read(NPCR),
	accusationH(S, W, R, NPCR),
	finishTurn.


accusationO(n) :-
	finishTurn.

accusationH(_,_,_,0) :- !.
accusationH(Suspect, Weapon, Room, Num) :-
	write('Who couldn\'t refute it? \n'),
	read(CR1),
	(hasCardSuspect(CR1, Suspect);
	assert(cantHaveSuspect(CR1, Suspect))),
	(hasCardWeapon(CR1, Weapon);
	assert(cantHaveWeapon(CR1, Weapon))),
	(hasCardRoom(CR1, Room);
	assert(cantHaveRoom(CR1, Room))),
	NewNum is Num-1,
	accusationH(Suspect, Weapon, Room, (NewNum)).

finishTurn :-
	nextPlayer,
	(correctGuess(_, _, _),!);
	playerTurn.

knownCards(User) :-
	write('How many cards do you have? (3-6)\n'),
	read(NC),
	number(NC),
	write('Suspect Choices = [plum, mustard, green, peacock, scarlet, white]\n'),
	write('Weapon Choices = [rope, leadpipe, knife, wrench, candlestick, pistol]\n'),
	write('Room Choices = [hall, lounge, diningroom, kitchen, ballroom, conservatory, billiardroom, library, study]\n'),
	knownCardsH(User, NC),
	(playerDoesntHave(User);true).

knownCardsH(User, 3) :-
	write('What is your first card?\n'),
	read(C1),
	hasCard(User, C1),
	write('What is your second card?\n'),
	read(C2),
	hasCard(User, C2),
	write('What is your third card?\n'),
	read(C3),
	hasCard(User, C3).

knownCardsH(User, 4) :-
	knownCardsH(User, 3),
	write('What is your fourth card?\n'),
	read(C4),
	hasCard(User, C4).

knownCardsH(User, 5) :-
	knownCardsH(User, 4),
	write('What is your fifth card?\n'),
	read(C5),
	hasCard(User, C5).

knownCardsH(User, 6) :-
	knownCardsH(User, 5),
	write('What is your sixth card?\n'),
	read(C5),
	hasCard(User, C5).



generatePlayers(6) :-
	write('Player Choices = [scarlet, mustard, white, green, peacock, plum]\n'),
	assert(player(scarlet)),
	assert(player(mustard)),
	assert(player(white)),
	assert(player(green)),
	assert(player(peacock)),
	assert(player(plum)).

generatePlayers(5) :-
	generatePlayers(6),
	write('Which player is not in the game? \n'),
	read(P1),
	suspect(P1),
	retract(player(P1)).


generatePlayers(4) :-
	generatePlayers(6),
	write('Who is the first missing character? \n'),
	read(P1),
	suspect(P1),
	retract(player(P1)),
	write('Who is the second missing character? \n'),
	read(P2),
	suspect(P2),
	retract(player(P2)).

generatePlayers(3) :-
	generatePlayers(4),
	write('Who is the third missing character? \n'),
	read(P3),
	suspect(P3),
	retract(player(P3)).

