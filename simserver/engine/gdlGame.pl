/* -*- Mode:Prolog; coding:UTF-8; -*- */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gameserver-Spielengine fuer GDL-Game
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(gdlGame,[initMatch_gdlGame/8, play_gdlGame/9, joinMatch_gdlGame/7]).

:- use_module(library(file_systems)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(remoteaiwrapper).
:- ensure_loaded(simutils).

:- dynamic gdl/2, actions/3.


% Nachtraegliches Betreten einer Partie (hier nicht moeglich).
%
% @param MatchId ID der Partie
% @param PlayerDetails Detaillierte Infos zu den neuen Spielern
% @param Oldworld Alter Weltzustand
% @param Newworld Neuer Weltzustand
% @param Views Sichten aller Spieler
% @param Round Rundenbezeichnung
% @param Phase Phasenbezeichnung
%
%% joinMatch_rok(+MatchID, +PlayerDetails, +Oldworld, -Newworld, -Views, -Round, -Phase) is det.
%
joinMatch_gdlGame(_MatchId,_PlayerDetails,_Oldworld,_Newworld,_Views, _Round, _Phase) :-
        throw('In diesem Spiel ist ein nachtraegliches Betreten nicht moeglich.').

% Initiiert ein GDL-Spiel.
%
% @param MatchID ID des Spiels
% @param MatchOptions Optionen (Key-Value-Liste), hier: Die Pfade zur GDL- und VIS-Datei
% @param PlayerList Liste der Spieler
% @param PlayerDetails Detaillierte Spielerinfos
% @param WorldOut Weltzustand fuer die Datenbank
% @param Views Sichten der Spieler auf das Spiel
% @param Round Rundenbezeichnung, hier: "1"
% @param Phase Phasenbezeichnung, hier: "default"
%
%% initMatch_rok(+MatchID, +MatchOptions, +PlayerList, +PlayerDetails, -WorldOut, -Views, -Round, -Phase) is det.
% 
initMatch_gdlGame(MatchId, MatchOptions, PlayerList, PlayerDetails,  WorldOut, Views, "1", "default") :-
        PlayerList = [Player1,Player2,Player3,Player4],
        PlayerDetails = [player(Detail1,_N1,_Role1),
                         player(Detail2,_N2,_Role2),
                         player(Detail3,_N3,_Role3),
                         player(Detail4,_N4,_Role4)],
        PlayerDetailDict = [Player1-Detail1,
                            Player2-Detail2,
                            Player3-Detail3,
                            Player4-Detail4],
        
        processMatchOptions(MatchId,MatchOptions,GdlCode,VisCode),
        
        processVisCode(VisCode,Vis),
        
        % fuellt alle 2500 Kartenpositionen mit leeren Platzhalterkarten
        putInvisibleCards(ViewCards0),
        
        % initialisiert die World-Variable
        World0 = [playerDetailDict-PlayerDetailDict,
                  vis-Vis,
                  viewEmpty-ViewCards0,
                  state-0],
        
        importGdlCode(World0,World01,MatchId,GdlCode),
        
        updateGdl(World01),
        worldget(state,World01,State),
        State1 is State + 1,
        worldput(state,World01,State1,World1),
        
        worldget(version,Vis,VisVersion),
        (worldget(gdlVersion,VisVersion,ii) ->
         updateViewCards(World1,player1,ViewCardsPlayer1),
         updateViewCards(World1,player2,ViewCardsPlayer2),
         updateViewCards(World1,player3,ViewCardsPlayer3),
         updateViewCards(World1,player4,ViewCardsPlayer4)
        ;
         updateViewCards(World1,general,ViewCards),
         ViewCardsPlayer1 = ViewCards,
         ViewCardsPlayer2 = ViewCards,
         ViewCardsPlayer3 = ViewCards,
         ViewCardsPlayer4 = ViewCards
        ),
        
        allowedActions(World1,player1,AllowedActionsPlayer1),
        allowedActions(World1,player2,AllowedActionsPlayer2),
        allowedActions(World1,player3,AllowedActionsPlayer3),
        allowedActions(World1,player4,AllowedActionsPlayer4),
        
        Player1View = view(player1,ViewCardsPlayer1,[],AllowedActionsPlayer1,[],[],undo(0,[]),[recipient('all players',[player2,player3,player4])]),
        Player2View = view(player2,ViewCardsPlayer2,[],AllowedActionsPlayer2,[],[],undo(0,[]),[recipient('all players',[player1,player3,player4])]),
        Player3View = view(player3,ViewCardsPlayer3,[],AllowedActionsPlayer3,[],[],undo(0,[]),[recipient('all players',[player1,player2,player4])]),
        Player4View = view(player4,ViewCardsPlayer4,[],AllowedActionsPlayer4,[],[],undo(0,[]),[recipient('all players',[player1,player2,player3])]),
        
        randomPlay(World1),
        
        
        % Outputs an Java
        WorldOut = World1,
        Views = [Player1View,Player2View,Player3View,Player4View],
        
        debprintln('finish InitMatch'),
        true.

% Fuehre einen GUI-Schritt aus, d.h. verarbeite die Aktion des Users.
%
% @param Player Spieler
% @param GuiAction Aktion auf der GUI
% @param MatchNumber Spielnummer (derzeit nicht verwendet)
% @param WorldIn Weltzustand vor dem Spielzug
% @param WorldOut Weltzustand nach dem Spielzug
% @param Views Sichten der Spieler auf das Spiel
% @param Matchstate Zustand des Spiels (laufend oder beendet mit Ergebnis)
% @param Round Rundenbezeichnung, hier: "1"
% @param Phase Phasenbezeichnung, hier: "default"
%
%% playJava(+Player, +GuiAction, +MatchNumber, +WorldIn, -WorldOut, -Views, -Matchstate, -Round, -Phase) is det.
%
play_gdlGame(Player, GuiAction, _MatchNumber, WorldIn, WorldOut, Views, Matchstate, "1", "default") :-
        worldget(vis,WorldIn,Vis),
        worldget(state,WorldIn,State),
        
        worldget(players,Vis,VisPlayers),
        worldget(Player,VisPlayers,PlayerGdl),
        
        (isGuiPlayer(WorldIn,Player) ->
         atom_term(GuiActionAtom,GuiAction),
         worldget(actions,Vis,VisActions),
         worldget(GuiActionAtom,VisActions,ActionAtom),
         atom_term(ActionAtom,Action),
         World0 = WorldIn
        ;
         aiActionHandler(WorldIn,World0,Player,Action)
        ),
        
        assert(actions(State,PlayerGdl,Action)),
        
        (allActions(World0) ->
         updateGdl(World0),
         State1 is State + 1,
         worldput(state,World0,State1,World1),
         
         % bei Spielende den Sieger auswerten
         (terminal(World1) ->
          findall(Role,role(World1,Role),Roles),
          delete(Roles,random,RolesNoRandom),
          playerPoints(World1,RolesNoRandom,PlayerPoints),
          keysort(PlayerPoints,PlayerPointsSort0),
          reverse(PlayerPointsSort0,PlayerPointsSort),
          matchstateResult(World1,PlayerPointsSort,MatchstateResult),
          Matchstate0 = matchstate(finished,MatchstateResult)
         ;
          Matchstate0 = matchstate(running(0),[])
         ),
         randomPlay(World1)
        ;
         World1 = World0,
         Matchstate0 = matchstate(running(0),[])
        ),
        
        worldget(version,Vis,VisVersion),
        (worldget(gdlVersion,VisVersion,ii) ->
         updateViewCards(World1,player1,ViewCardsPlayer1),
         updateViewCards(World1,player2,ViewCardsPlayer2),
         updateViewCards(World1,player3,ViewCardsPlayer3),
         updateViewCards(World1,player4,ViewCardsPlayer4)
        ;
         updateViewCards(World1,general,ViewCards),
         ViewCardsPlayer1 = ViewCards,
         ViewCardsPlayer2 = ViewCards,
         ViewCardsPlayer3 = ViewCards,
         ViewCardsPlayer4 = ViewCards
        ),
        
        % gibt die erlaubten Aktionen der Spieler wieder
        (Matchstate0 = matchstate(running(0),[]) ->
         allowedActions(World1,player1,AllowedActionsPlayer1),
         allowedActions(World1,player2,AllowedActionsPlayer2),
         allowedActions(World1,player3,AllowedActionsPlayer3),
         allowedActions(World1,player4,AllowedActionsPlayer4)
        ; % keine Aktionen bei Spielende
         AllowedActionsPlayer1 = [],
         AllowedActionsPlayer2 = [],
         AllowedActionsPlayer3 = [],
         AllowedActionsPlayer4 = []
        ),
                
        Player1View = view(player1,ViewCardsPlayer1,[],AllowedActionsPlayer1,[],[],undo(0,[]),[recipient('all players',[player2,player3,player4])]),
        Player2View = view(player2,ViewCardsPlayer2,[],AllowedActionsPlayer2,[],[],undo(0,[]),[recipient('all players',[player1,player3,player4])]),
        Player3View = view(player3,ViewCardsPlayer3,[],AllowedActionsPlayer3,[],[],undo(0,[]),[recipient('all players',[player1,player2,player4])]),
        Player4View = view(player4,ViewCardsPlayer4,[],AllowedActionsPlayer4,[],[],undo(0,[]),[recipient('all players',[player1,player2,player3])]),
                
        
        % Outputs an Java
        WorldOut = World1,
        Views = [Player1View,Player2View,Player3View,Player4View],
        Matchstate = Matchstate0,
        
        % gibt den Matchstate im Terminal aus (TODO: an die Oberflaeche bringen)
        debprintln('Matchstate:'-Matchstate),
        true.



% Fuellt alle Position mit unsichtbaren Platzhalterkarten.
%
% @param ViewCards Key-Value-Liste mit allen Positionen als Keys und Platzhalterkarten als Value
%
%% putInvisibleCards(-ViewCards) is det.
%
putInvisibleCards(ViewCards) :-
        getAllPositions(AllPositions),
        putInvisibleCards(ViewCards,AllPositions).

putInvisibleCards([],[]).
putInvisibleCards([PositionAtom-card(none-0)|ViewCards],[Position|Positions]) :-
        atom_number(PositionAtom,Position),
        putInvisibleCards(ViewCards,Positions).

% Gibt alle moeglichen Position-IDs zurueck (50x50 Grid).
%
% @param AllPositions Liste mit 10101-10150,10201-10250,...,15001-15050
%
%% getAllPositions(-AllPositions) is det.
%
getAllPositions(AllPositions) :-
        range(1,50,Range),
        getAllPositions(AllPositions,Range,Range).
getAllPositions([],[],_).
getAllPositions([],_,[]).
getAllPositions(AllPositions,[X|Xs],Ys) :-
        getAllPositions1(Positions1,X,Ys),
        getAllPositions(Positions2,Xs,Ys),
        append([Positions1,Positions2],AllPositions).

getAllPositions1(_,_,[]).
getAllPositions1([Z|Positions],X,[Y|Ys]) :-
        Z1 is 10000 + Y,
        Z2 is 100 * X,
        Z is Z1 + Z2,
        getAllPositions1(Positions,X,Ys).


% Erstellt Fakten des naechsten Spielzustands anhand der init/next Praedikate der GDL.
%
% @param World Weltzustand
%
%% updateGdl(+World) is det.
%
updateGdl(World) :-
        (worldget(state,World,0) ->
         findall(Init,init(World,Init),Terms)
        ;
         findall(Next,next(World,Next),Terms)
        ),
        updateGdl(World,Terms).

updateGdl(_,[]).
updateGdl(World,[Term|Terms]) :-
        worldget(state,World,State),
        State1 is State + 1,
        assert(gdl(State1,Term)),
        updateGdl(World,Terms).


% Aktualisiert die ViewCards des Spielers.
%
% @param World Weltzustand
% @param Player Spieler
% @param ViewCards Key-Value-Liste der sichtbaren Karten
%
%% updateViewCards(+World, +Player, -ViewCards) is det.
%
updateViewCards(World,Player,ViewCards) :-
        worldget(vis,World,Vis),
        
        worldget(players,Vis,VisPlayers),
        (worldget(Player,VisPlayers,PlayerGdl) -> % spezifische View fuer Spieler
         findall(SeesXml,sees_xml(World,PlayerGdl,SeesXml),Facts,[])
        ; % allgemeine View fuer Spieler
         worldget(state,World,State),
         findall(Gdl,gdl(State,Gdl),Facts,[])
        ),
        
        worldget(positionsuitvalue,Vis,VisPSV),
        worldget(cardPosition,VisPSV,CardPosition),
        worldget(cardSuit,VisPSV,CardSuit),
        worldget(cardValue,VisPSV,CardValue),
        
        splitVisFact(CardPosition,CardPositions),
        splitVisFact(CardSuit,CardSuits),
        splitVisFact(CardValue,CardValues),
                
        getFacts(CardPositions,Facts,PositionFacts0),
        delete(PositionFacts0,[],PositionFacts),
        getFacts(CardSuits,Facts,SuitFacts0),
        delete(SuitFacts0,[],SuitFacts),
        getFacts(CardValues,Facts,ValueFacts0),
        delete(ValueFacts0,[],ValueFacts),
                
        getViewCards(World,PositionFacts,SuitFacts,ValueFacts,ViewCardsNew),
        
        worldget(viewEmpty,World,ViewCardsEmpty),
        emptyWithNewViewCards(ViewCardsEmpty,ViewCardsNew,ViewCards).

% Teilt einen Fakt anhand "+" und gibt die resultierenden Fakten in einer Liste zurueck.
%
% @param VisFact Atom der Fakten mit "+" verbunden
% @param VisFacts Fakten aufgeteilt und in einer Liste
%
%% splitVisFact(+VisFact, -VisFacts) is det.
%
splitVisFact('',[]).
splitVisFact(VisFact,VisFacts) :-
        atom_codes(VisFact,VisFactCode),
        splitVisFact1(VisFactCode,VisFacts).
splitVisFact1(VisFact,[VisFacts]) :-
        \+member(43,VisFact),
        atom_codes(VisFactAtom,VisFact),
        atom_term(VisFactAtom,VisFacts).
splitVisFact1(VisFact,[PrefixTerm|VisFacts]) :-
        sublist(VisFact,[43],PrefixLength,PartLength,_SuffixLength),
        append_length(Prefix,PartSuffix,VisFact,PrefixLength),
        append_length(_Part,Suffix,PartSuffix,PartLength),
        atom_codes(PrefixAtom,Prefix),
        atom_term(PrefixAtom,PrefixTerm),
        splitVisFact1(Suffix,VisFacts).

% Gibt zutreffende Fakten zurueck.
%
% @param VisFacts Bezeichnung des Fakts aus dem VIS
% @param FactsIn Datenbasis an Fakten
% @param FactsOut Zutreffende Fakten
%
%% getFacts(+VisFacts, +FactsIn, -FactsOut) is det.
%
getFacts([],_,[]).
getFacts(_,[],[]).
getFacts([head(VisFactNumber_n)],[FactIn|FactsIn],[FactOut|FactsOut]) :-
        n_numberToNumber(VisFactNumber_n,VisFactNumber),
        (VisFactNumber = 0 ->
         FactIn =.. [FactOut|_]
        ;
         FactIn =.. [_|FactInTerms],
         nth1(VisFactNumber,FactInTerms,FactOut)
        ),
        getFacts([head(VisFactNumber_n)],FactsIn,FactsOut).
getFacts(VisFacts,[FactIn|FactsIn],[FactOut|FactsOut]) :-
        getFacts1(VisFacts,FactIn,FactOut0),
        combinePlus(FactOut0,FactOut),
        getFacts(VisFacts,FactsIn,FactsOut).

getFacts1([],_,[]).
getFacts1([VisFact|VisFacts],FactIn,[FactOut|FactsOut]) :-
        VisFact =.. [VisFactName_n,VisFactNumber_n],
        n_numberToNumber(VisFactNumber_n,VisFactNumber),
        FactIn =.. [VisFactName_n|FactInTerms],
        nth1(VisFactNumber,FactInTerms,FactOut),
        getFacts1(VisFacts,FactIn,FactsOut).
getFacts1([_|VisFacts],FactIn,FactsOut) :-
        getFacts1(VisFacts,FactIn,FactsOut).

% Erstellt ein Atom, das alle Fakten aneinanderreiht und mit einem "+" verbindet.
%
% @param Facts Liste an Fakten
% @param FactsCombined Fakten aus Facts mit einem "+" verbunden als Atom
%
%% combinePlus(+Facts, -FactsCombined) is det.
%
combinePlus([],[]).
combinePlus([Fact],Fact).
combinePlus([Fact|Facts],FactsCombined) :-
        combinePlus(Facts,FactsCombined0),
        atom_concat(Fact,'+',FactPlus),
        atom_concat(FactPlus,FactsCombined0,FactsCombined).

% Erzeugt die Key-Value-Liste ViewCards anhand der Positionen, Symbole und Zahlen.
%
% @param World Weltzustand
% @param Positions Position der Karte
% @param Suits Symbol der Karte
% @param Values Zahl der Karte
% @param ViewCards Key-Value-Liste mit Positionen, Symbolen und Zahlen der Karten
%
%% getViewCards(+World, +Positions, +Suits, +Values, -ViewCards) is det.
%
getViewCards(_,[],[],[],[]):-!.
getViewCards(World,Positions,Suits,Values,[Position1-card(Card)|ViewCards]) :-
        worldget(vis,World,Vis),
        (Positions = [] ->
         Position1 = 12525,
         PositionFacts = []
        ;
         Positions = [Position|PositionFacts],
         worldget(positions,Vis,VisPositions),
         worldget(Position1,VisPositions,Position)
        ),
        (Suits = [] ->
         Suit1 = c,
         SuitFacts = []
        ;
         Suits = [Suit|SuitFacts],
         worldget(suits,Vis,VisSuits),
         worldget(Suit1,VisSuits,Suit)
        ),
        (Values = [] ->
         Value1 = a,
         ValueFacts = []
        ;
         Values = [Value|ValueFacts],
         worldget(values,Vis,VisValues),
         worldget(Value1,VisValues,Value)
        ),
        
        (Suit1 = none ->
         Card = hiddenCard
        ;
         (Value1 = hidden ->
          Card = hiddenCard
         ;
          Card = Suit1-Value1
         )
        ),
        getViewCards(World,PositionFacts,SuitFacts,ValueFacts,ViewCards).

% Gibt die kompletten Viewcards zurueck mit den in ViewCardsUpdate bestimmten Karten und dem Rest Platzhalterkarten.
%
% @param ViewCardsEmpty Viewcards mit Platzhalterkarten
% @param ViewCardsUpdate Key-Value-Liste mit Viewcards die dargestellt werden sollen
% @param ViewCardsNew Viewcards mit darzustellenden Karten und dem Rest Platzhalterkarten
%
%% emptyWithNewViewCards(+ViewCardsEmpty, +ViewCardsUpdate, -ViewCardsNew) is det.
%
emptyWithNewViewCards(ViewCards,[],ViewCards).
emptyWithNewViewCards(ViewCardsOld,[Key-Value|ViewCardsUpdate],ViewCardsNew) :-
        worldput(Key,ViewCardsOld,Value,ViewCardsNew1),
        emptyWithNewViewCards(ViewCardsNew1,ViewCardsUpdate,ViewCardsNew).


% Gibt die erlaubten Aktionen für den Spieler zurueck.
%
% @param World Weltzustand
% @param Player Spieler
% @param AllowedActions Liste der erlaubten Aktionen
%
%% allowedActions(+World, +Player, -AllowedActions) is det.
%
allowedActions(World,Player,AllowedActions) :-
        worldget(state,World,State),
        worldget(vis,World,Vis),
        worldget(players,Vis,VisPlayers),
        worldget(Player,VisPlayers,PlayerGdl),
        (PlayerGdl = '' ->
         AllowedActions = []
        ;
         (actions(State,PlayerGdl,_) ->
          AllowedActions = []
         ;
          findall(Action,legal(World,PlayerGdl,Action),Actions),
          allowedActions1(World,Actions,AllowedActions)
         )
        ).

allowedActions1(_,[],[]).
allowedActions1(World,[Action|Actions],[ActionNumber-[click]|AllowedActions]) :-
        worldget(vis,World,Vis),
        (atom_term(ActionAtom,Action) ->
         true
        ;
         ActionAtom = Action
        ),
        worldget(actions,Vis,VisActions),
        worldget(EngineActionAtom,VisActions,ActionAtom),
        atom_term(EngineActionAtom,EngineActionTerm),
        EngineActionTerm =.. [click,ActionNumber],
        allowedActions1(World,Actions,AllowedActions).


% Führt zufällige Aktion für den Spieler "random" aus.
%
% @param World Weltzustand
%
%% randomPlay(+World) is det.
%
randomPlay(World) :-
        worldget(state,World,State),
        findall(Action,legal(World,random,Action),Actions,[]),
        (Actions = [] -> % GDL-I oder keine random-role
         true
        ;
         now(Time),
         RandomState is Time mod 30000,
         setrand(RandomState),
         random_member(RandomAction,Actions),
         assert(actions(State,random,RandomAction))
        ).


% Wahr, wenn Spieler menschlich ist.
%
% @param World Weltzustand
% @param Player Spieler
%
%% isGuiPlayer(+World, +Player) is det.
%
isGuiPlayer(World,Player) :-
        worldget(playerDetailDict,World,PlayerDetailDict),
        worldget(Player,PlayerDetailDict,human).

% Importiert KI und gibt KI-Aktion zurueck.
%
% @param WorldIn Weltzustand
% @param WorldOut Weltzustand mit neuer Knowledgebase
% @param Player Spieler
% @param Action Aktion der KI
%
%% aiActionHandler(+WorldIn, -WorldOut, +Player, -Action) is det.
%
aiActionHandler(WorldIn,WorldOut,Player,Action) :-
        worldget(vis,WorldIn,Vis),
        worldget(players,Vis,VisPlayers),
        worldget(Player,VisPlayers,PlayerGdl),
        
        createAiWorld(WorldIn,Player,AiWorld),
        
        atom_concat(Player,'Kb',PlayerKb),
        (worldget(PlayerKb,WorldIn,KbIn) ->
         true
        ;
         KbIn = []
        ),
        
        worldget(playerDetailDict,WorldIn,PlayerDetailDict),
        worldget(Player,PlayerDetailDict,PlayerDetails),        
        (PlayerDetails = localai(AiName) ->
         aifolder(Aifolder), % Ordner, in dem die AI-Spieler liegen (atom, incl. Slash am Ende)
         atom_concat('z_modulename_',AiName,AiModule),
         atoms_concat([Aifolder,AiName,'/z_modulename_',AiName],AiModulePath),
         atoms_concat([Aifolder,AiName,'/'],AiPath),
         current_directory(Serverpath,AiPath), % aktuellen Working Dir merken, aber auf AiPath setzen
         use_module(AiModulePath),
         atom_concat('z_',AiName,AiPredicate),
         AiGoal =.. [AiPredicate,_Version,AiWorld,KbIn,AiAction,KbOut],
         (AiModule:AiGoal ->
          current_directory(_,Serverpath)       % zurueck zum Server-Verzeichnis
         ;
          current_directory(_,Serverpath),      % zurueck zum Server-Verzeichnis
          throw(gameException('LocalAiFailed'))
         ),
         true
        ;
         (PlayerDetails = remoteai(Host,Port) ->
          (Port = 3142 ->
           Protocol = socket
          ;
           Protocol = webservice
          )
         ;
          PlayerDetails = remoteai(Host,Port,Protocol)
         ),
         remoteAiHost_port_protocol_playersWorld_oldkb_newkb_action(Host,Port,Protocol,AiWorld,KbIn,KbOut,AiAction)
        ),
        !,
        
        (AiAction = timeoutError ->
         throw(gameException('{AITimeOut}'))
        ;
         (legal(WorldIn,PlayerGdl,AiAction) ->
          Action = AiAction
         ;
          throw(gameException('Die ausgefuehrte AI-Aktion ist in diesem Zustand nicht gueltig.'-AiAction))
         )
        ),
        
        worldput(PlayerKb,WorldIn,KbOut,WorldOut).

% Erstellt World fuer KI-Spieler
%
% @param World Weltzustand
% @param Player Spieler
% @param AiWorld Welt mit Informationen, die der KI zur Verfuegung stehen
%
%% createAiWorld(+World, +Player, -AiWorld) is det.
%
createAiWorld(World,Player,AiWorld) :-
        worldget(state,World,State),
        worldget(vis,World,Vis),
        worldget(players,Vis,VisPlayers),
        worldget(Player,VisPlayers,PlayerGdl),
        
        worldget(gdlPath,World,GdlPath),
        AiWorld0 = [gdlPath-GdlPath,
                   gdl-[],
                   allowed-[],
                   player-PlayerGdl],
        
        worldget(version,Vis,VisVersion),
        (worldget(gdlVersion,VisVersion,i) ->
         findall(Gdl,gdl(State,Gdl),Gdls,[]),
         worldput(gdl,AiWorld0,Gdls,AiWorld1)
        ;
         State1 is State - 1,
         worldput(state,World,State1,World0),
         findall(See,sees(World0,PlayerGdl,See),Sees,[]),
         worldput(gdl,AiWorld0,Sees,AiWorld1)
        ),
        
        findall(Action,legal(World,PlayerGdl,Action),Actions,[]),
        worldput(allowed,AiWorld1,Actions,AiWorld).


% Wahr, wenn alle Spieler eine Aktion im aktuellen Zustand ausgewaehlt haben.
%
% @param World Weltzustand
%
%% allActions(+World) is det.
%
allActions(World) :-
        findall(Role,role(World,Role),Roles),
        allActions(World,Roles).

allActions(_,[]).
allActions(World,[Role|Roles]) :-
        worldget(state,World,State),
        actions(State,Role,_),
        allActions(World,Roles).


% Gibt eine Liste zurueck mit den Punkten die jeder Spieler erreicht hat.
%
% @param World Weltzustand
% @param Roles Spieler
% @param PlayerPoints Key-Value-Liste mit Punkte-Spieler
%
%% playerPoints(+World, +Roles, -PlayerPoints) is det.
%
playerPoints(_,[],[]).
playerPoints(World,[Role|Roles],[Points-Role|PlayerPoints]) :-
        goal(World,Role,Points_n),
        n_numberToNumber(Points_n,Points),
        playerPoints(World,Roles,PlayerPoints).


% Gibt die Ergebnisliste zurueck, die bei einer beendeten Partie dem Matchstate gegeben wird.
%
% @param World Weltzustand
% @param PlayerPoints Key-Value-Liste mit Punkte-Spieler
% @param MatchstateResult Liste mit allen Spielern und zugehörigen Punkten sowie won/lost
%
%% matchstateResult(+World, +PlayerPoints, -MatchstateResult) is det.
%
matchstateResult(_,[],[]).
matchstateResult(World,[Points-PlayerGdl|PlayerPoints],[player(Player,Result,Points)|MatchstateResult]) :-
        worldget(vis,World,Vis),
        worldget(players,Vis,VisPlayers),
        worldget(Player,VisPlayers,PlayerGdl),
        (Points > 50 ->
         Result = won
        ;
         Result = lost
        ),
        matchstateResult(World,PlayerPoints,MatchstateResult).



% Loescht "n_" aus zu Atomen gezwungenen Zahlen.
%
% @param N_number Atom der Form "n_"+Number
% @param Number Zahl als number
%
%% n_numberToNumber(+N_number, -Number) is det.
%
n_numberToNumber(N_number,Number) :-
        atom_codes(N_number,N_numberCode),
        delete_n_(N_numberCode,NumberCode),
        number_codes(Number,NumberCode).
%
delete_n_([],[]).
delete_n_([110,95|Points],Numbers) :-
        delete_n_(Points,Numbers),!.
delete_n_([Number|Points],[Number|Numbers]) :-
        delete_n_(Points,Numbers),!.



/*------------------------ GDL Predicates ------------------------*/
% GDL: distinct. Wahr, wenn X sich von Y unterscheidet.
%
% @param World Weltzustand
% @param X 1. Atom
% @param Y 2. Atom
%
%% distinct(+World, +X, +Y) is det.
%
distinct(_,X,Y) :-
        dif(X,Y).

% GDL: true. Wahr, wenn Term im aktuellen Zustand existiert.
%
% @param World Weltzustand
% @param Term Fakt
%
%% true(+World, +Term) is det.
%
true(World,Term) :-
        worldget(state,World,State),
        gdl(State,Term).

% GDL: does. Wahr, wenn der Spieler Player im aktuellen Zustand Action als seine Aktion gewaehlt hat.
%
% @param World Weltzustand
% @param Player Spieler
% @param Action Aktion
%
%% true(+World, +Term, +Action) is det.
%
does(World,Player,Action) :-
        worldget(state,World,State),
        actions(State,Player,Action).
/*---------------------- End: GDL Predicates ---------------------*/

/*---------------------- GDL/VIS processing ----------------------*/
% Verarbeitet die Matchoptionen: gibt die Codes der angegebenen Files aus.
%
% @param MatchId ID des Spiels
% @param MatchOptions Beinhalten die Pfade zu den GDL- und VIS-Dateien
% @param GdlCode GDL-Code als String
% @param VisCode VIS-Code als String
%
%% processMatchOptions(+MatchId, +MatchOptions, -GdlCode, -VisCode) is det.
%
processMatchOptions(MatchId,MatchOptions,GdlCode,VisCode) :-
        % get GDL-Code
        (worldget(gdlfile, MatchOptions, GdlfileName) ->       
         match_file_fullpath(MatchId, GdlfileName, FullGdlfileName),
         file2string(FullGdlfileName,GdlCode)
        ;
         throw('Keine GDL-File angegeben!')
        ),
        % get VIS-Code
        (worldget(visfile,MatchOptions,VisfileName) ->        
         match_file_fullpath(MatchId,VisfileName,FullVisfileName),
         file2string(FullVisfileName,VisCode)
        ;
         throw('Keine VIS-File angegeben!')
        ).

% Bestimmt zu Match-ID und Dateiname vollstaendigen Pfad und wirft Ausnahmen, falls Uploadverzeichnis oder Datei nicht existieren.
%
% @param MatchId ID des Spiels
% @param Filename Dateiname
% @param Fullpath Kompletter Pfad zur Datei
%
%% match_file_fullpath(+MatchId, +Filename, -Fullpath) is det.
%
match_file_fullpath(MatchId,Filename,Fullpath) :-
        atom_number(MatchIdAtom,MatchId),
        (Filename = '' ->
         throw('Benoetigtes File nicht ausgewaehlt!!!')
        ;
         true
        ),
        uploadfolder(Ufolder),
        (directory_member_of_directory(Ufolder,MatchIdAtom,_) ->
         true
        ;
         throw('Verzeichnis mit hochgeladenen Dateien existiert nicht!')
        ),
        atom_concat(Ufolder,MatchIdAtom,FullDirPathAtom),
        file_members_of_directory(FullDirPathAtom,Set),
        (worldget(Filename,Set,Fullpath) ->
         true
        ;
         throw('Datei existiert nicht!')
        ).

% Speichert verarbeiteten GDL-Code in den temporären Matchfolder und importiert ihn.
%
% @param WorldIn Weltzustand
% @param WorldOut Weltzustand mit Pfad zur GDL-Datei
% @param MatchId ID des Spiels
% @param GdlCode GDL-Code als String
%
%% importGdlCode(+WorldIn, -WorldOut, +MatchId, +GdlCode) is det.
%
importGdlCode(WorldIn,WorldOut,MatchId,GdlCode) :-
        gdlToProlog(GdlCode,PrologCode),
        atom_codes(PrologAtom,PrologCode),
        
        uploadfolder(Ufolder),
        atom_number(MatchIdAtom,MatchId),
        PrologFile = '/gdlAsProlog.pl',        
        atom_concat(Ufolder,MatchIdAtom,FullDirPathAtom),
        atom_concat(FullDirPathAtom,PrologFile,FullPathFile),
        
        open(FullPathFile,write,StreamWrite),
        write(StreamWrite,PrologAtom),
        close(StreamWrite),
        
        use_module(FullPathFile),
        
        worldput(gdlPath,WorldIn,FullPathFile,WorldOut).

% Verarbeitet GDL-Code, sodass er als Prolog-Code verwendet werden kann.
%
% @param GdlCode GDL-Code als String
% @param PrologCode Zu Prolog-Code transformierter GDL-Code
%
%% gdlToProlog(+GdlCode, -PrologCode) is det.
%
gdlToProlog(GdlCode,PrologCode) :-
        select(-1,GdlCode,GdlCode0),            % entfernt die -1 am Ende
        code_deleteComments(GdlCode0,Code1),    % loescht Kommentarzeilen
        code_replace(38,Code1,44,Code2),        % ersetzt "&" durch ","
        code_replace(126,Code2,[92,43],Code3),  % ersetzt "~" durch "\+"
        code_numbersToAtoms(Code3,Code4),       % fuegt vor jeder Zahl "n_" ein
        code_deleteEmptyRows(Code4,Code5),      % loescht leere Zeilen
        code_replace(32,Code5,[],Code6),        % loescht Leerstellen
        code_addDots(Code6,PrologCode0),        % fuegt Punkte ein
        code_addWorldVar(PrologCode0,PrologCode).

% Transformiert erhaltenen Visualisierungscode in ein VisualizationDictionary um.
%
% @param VisCode VIS-Code als String
% @param VisDict Key-Value-Liste aus VisCode
%
%% processVisCode(+VisCode, -VisDict) is det.
%
processVisCode(VisCode,VisDict) :-
        select(-1,VisCode,Code0),            % entfernt die -1 am Ende       
        code_deleteComments(Code0,Code1),    % loescht Kommentarzeilen
        code_deleteEmptyRows(Code1,Code),    % loescht leere Zeilen
        getVisDict(Code,VisDict).
%
getVisDict(VisCode,VisDict) :-
        getVisKeyDict(VisCode,"#version",VersionDict),
        getVisKeyDict(VisCode,"#players",PlayersDict),
        getVisKeyDict(VisCode,"#positionsuitvalue",PositionsuitvalueDict),
        getVisKeyDict(VisCode,"#positions",PositionsDict),
        getVisKeyDict(VisCode,"#suits",SuitsDict),
        getVisKeyDict(VisCode,"#values",ValuesDict),
        getVisKeyDict(VisCode,"#actions",ActionsDict),
        VisDict = [version-VersionDict,
                   players-PlayersDict,
                   positionsuitvalue-PositionsuitvalueDict,
                   positions-PositionsDict,
                   suits-SuitsDict,
                   values-ValuesDict,
                   actions-ActionsDict].
%
getVisKeyDict(VisCode,Key,KeyDict) :-
        sublist(VisCode,Key,PrefixLength1,PartLength1,_SuffixLength1),
        append_length(_Prefix1,PartSuffix1,VisCode,PrefixLength1),
        append_length(_Part1,Suffix1,PartSuffix1,PartLength1),
        sublist(Suffix1,[35],PrefixLength2,_PartLength2,_SuffixLength2),
        append_length(Prefix2,_PartSuffix2,Suffix1,PrefixLength2),        
        Prefix2 = [10|KeyCode],        
        getVisKeyDict(KeyCode,KeyDict).
%
getVisKeyDict([],[]).
getVisKeyDict(VisCode,[PrefixAtom-SuffixAtom]) :-
        \+member(10,VisCode),
        sublist(VisCode,[60,45],PrefixLength,PartLength,_SuffixLength),
        append_length(Prefix,PartSuffix,VisCode,PrefixLength),
        append_length(_Part,Suffix2,PartSuffix,PartLength),
        code_numbersToAtoms(Suffix2,Suffix3),
        atom_codes(PrefixAtom,Prefix),
        atom_codes(SuffixAtom,Suffix3).
getVisKeyDict(VisCode,[Prefix2Atom-Suffix2Atom|VisDict]) :-
        sublist(VisCode,[10],PrefixLength1,PartLength1,_SuffixLength1),
        append_length(Prefix1,PartSuffix1,VisCode,PrefixLength1),
        append_length(_Part1,Suffix1,PartSuffix1,PartLength1),
        sublist(Prefix1,[60,45],PrefixLength2,PartLength2,_SuffixLength2),
        append_length(Prefix2,PartSuffix2,Prefix1,PrefixLength2),
        append_length(_Part2,Suffix2,PartSuffix2,PartLength2),
        code_numbersToAtoms(Suffix2,Suffix3),
        atom_codes(Prefix2Atom,Prefix2),
        atom_codes(Suffix2Atom,Suffix3),
        getVisKeyDict(Suffix1,VisDict).


% Loescht ab allen ";" bis Zeilenende (Kommentare).
%
% @param CodeIn Eingehender Code
% @param CodeOut Verarbeiteter Code
%
%% code_deleteComments(+CodeIn, -CodeOut) is det.
%
code_deleteComments([],[]).
code_deleteComments(Code,Code) :-
        \+member(59,Code).
code_deleteComments(CodeIn,CodeOut) :-
        sublist(CodeIn,[59],PrefixLength1,_PartLength1,_SuffixLength1),
        append_length(Prefix1,Suffix1,CodeIn,PrefixLength1),
        sublist(Suffix1,[10],PrefixLength2,PartLength2,_PartLength2),
        AppendPosition is PrefixLength2 + PartLength2,
        append_length(_Prefix2,Suffix2,Suffix1,AppendPosition),
        code_deleteComments(Suffix2,Code),
        append([Prefix1,Code],CodeOut).

% Ersetzt Code X durch Code Y.
%
% @param X Code, der ersetzt werden soll
% @param CodeIn Eingehender Code
% @param Y Code, durch den ersetzt wird
% @param CodeOut Verarbeiteter Code
%
%% code_replace(+X, +CodeIn, +Y, -CodeOut) is det.
%
code_replace(_,[],_,[]).
code_replace(X,Code,_,Code) :-
        (is_list(X) -> Xs = X ; Xs = [X]),
        \+sublist(Code,Xs,_,_,_).
code_replace(X,CodeIn,Y,CodeOut) :-
        (is_list(X) -> Xs = X ; Xs = [X]),
        (is_list(Y) -> Ys = Y ; Ys = [Y]),
        sublist(CodeIn,Xs,PrefixLength,PartLength,_SuffixLength),
        append_length(Prefix,PartSuffix,CodeIn,PrefixLength),
        append_length(_Part,Suffix,PartSuffix,PartLength),
        code_replace(Xs,Suffix,Ys,Code),
        append([Prefix,Ys,Code],CodeOut).

% Ersetzt Zahlen "0"-"9" durch "n_0"-"n_9" (zwingt Zahlen zu Atome).
%
% @param CodeIn Eingehender Code
% @param CodeOut Verarbeiteter Code
%
%% code_numbersToAtoms(+CodeIn, -CodeOut) is det.
%
code_numbersToAtoms(CodeIn,CodeOut) :-
        code_numbersToAtoms(48,57,CodeIn,CodeOut).
code_numbersToAtoms(LastNumber,LastNumber,CodeIn,CodeOut) :-
        code_replace(LastNumber,CodeIn,[110,95,LastNumber],CodeOut).
code_numbersToAtoms(Number,LastNumber,CodeIn,CodeOut) :-
        code_replace(Number,CodeIn,[110,95,Number],Code),
        NextNumber is Number +1,
        code_numbersToAtoms(NextNumber,LastNumber,Code,CodeOut).

% Loescht leere Zeilen am Anfang, Ende und nach leeren Zeilen.
%
% @param CodeIn Eingehender Code
% @param CodeOut Verarbeiteter Code
%
%% code_deleteEmptyRows(+CodeIn, -CodeOut) is det.
%
code_deleteEmptyRows(CodeIn,CodeOut) :-
        CodeIn = [10|Rest],
        code_deleteEmptyRows(Rest,CodeOut).
code_deleteEmptyRows(CodeIn,CodeOut) :-
        last(Rest,10,CodeIn),
        code_deleteEmptyRows(Rest,CodeOut).
code_deleteEmptyRows(Code,Code) :-
        \+nextto(10,10,Code).
code_deleteEmptyRows(CodeIn,CodeOut) :-
        code_replace([10,10],CodeIn,10,Code),
        code_deleteEmptyRows(Code,CodeOut).

% Fuegt Punkte an Zeilenenden ohne "-" oder "," ein.
%
% @param CodeIn Eingehender Code
% @param CodeOut Verarbeiteter Code
%
%% code_addDots(+CodeIn, -CodeOut) is det.
% 
code_addDots([],[]).
code_addDots(Code,Code) :-
        last(_,46,Code),
        \+member(10,Code).
code_addDots(CodeIn,CodeOut) :-
        append(CodeIn,[46],CodeOut),
        \+member(10,CodeIn).
code_addDots(CodeIn,CodeOut) :-
        sublist(CodeIn,[10],PrefixLength,PartLength,_SuffixLength),
        append_length(Prefix,PartSuffix,CodeIn,PrefixLength),
        append_length(Part,Suffix,PartSuffix,PartLength),
        last(Prefix,Last),
        dif(Last,45),
        dif(Last,44),
        code_addDots(Suffix,Code),
        append([Prefix,[46],Part,Code],CodeOut).
code_addDots(CodeIn,CodeOut) :-
        sublist(CodeIn,[10],PrefixLength,PartLength,_SuffixLength),
        append_length(Prefix,PartSuffix,CodeIn,PrefixLength),
        append_length(Part,Suffix,PartSuffix,PartLength),
        code_addDots(Suffix,Code),
        append([Prefix,Part,Code],CodeOut).

% Fuegt jedem Literal die Variable "World" als ersten Term hinzu.
%
% @param CodeIn Eingehender Code
% @param CodeOut Verarbeiteter Code
%
%% code_addWorldVar(+CodeIn, -CodeOut) is det.
% 
code_addWorldVar([],[]).
code_addWorldVar(Code,Code) :-
        \+member(10,Code),
        subseq0(Code,"World").
code_addWorldVar(CodeIn,CodeOut) :-
        \+member(10,CodeIn),
        code_addWorldVar_check(CodeIn,CodeOut).
code_addWorldVar(CodeIn,CodeOut) :-
        sublist(CodeIn,[10],PrefixLength,PartLength,_SuffixLength),
        append_length(Prefix,PartSuffix,CodeIn,PrefixLength),
        append_length(Part,Suffix,PartSuffix,PartLength),
        code_addWorldVar_check(Prefix,Prefix1),
        code_addWorldVar(Suffix,Code),
        append([Prefix1,Part,Code],CodeOut).
%
code_addWorldVar_check([],[]).
code_addWorldVar_check(CodeIn,CodeOut) :-
        \+member(40,CodeIn),
        sublist(CodeIn,[58,45],PrefixLength,PartLength,_SuffixLength),
        append_length(Prefix,PartSuffix,CodeIn,PrefixLength),
        append_length(Part,Suffix,PartSuffix,PartLength),
        append([Prefix,"(World)",Part,Suffix],CodeOut).
code_addWorldVar_check(CodeIn,CodeOut) :-
        \+member(40,CodeIn),
        sublist(CodeIn,[44],PrefixLength,PartLength,_SuffixLength),
        append_length(Prefix,PartSuffix,CodeIn,PrefixLength),
        append_length(Part,Suffix,PartSuffix,PartLength),
        append([Prefix,"(World)",Part,Suffix],CodeOut).
code_addWorldVar_check(CodeIn,CodeOut) :-
        \+member(40,CodeIn),
        sublist(CodeIn,[46],PrefixLength,PartLength,_SuffixLength),
        append_length(Prefix,PartSuffix,CodeIn,PrefixLength),
        append_length(Part,Suffix,PartSuffix,PartLength),
        append([Prefix,"(World)",Part,Suffix],CodeOut).
code_addWorldVar_check(CodeIn,CodeOut) :-
        code_addWorldVar_bracket(CodeIn,CodeOut).
%
code_addWorldVar_bracket([],[]).
code_addWorldVar_bracket(Code,Code) :-
        \+member(40,Code).
code_addWorldVar_bracket(CodeIn,CodeOut) :-
        sublist(CodeIn,[40],PrefixLength,PartLength,_SuffixLength),
        append_length(Prefix,PartSuffix,CodeIn,PrefixLength),
        append_length(Part,Suffix,PartSuffix,PartLength),
        append([Prefix,Part,"World,",Suffix],CodeOut).
/*------------------- End: GDL/VIS processing --------------------*/

/*---------------------------- Utility ---------------------------*/
% Transformiert Atom zu Term oder Term zu Atom.
%
% @param Atom Vom Typ Atom
% @param Term Vom Typ Term
%
%% atom_term(?Atom, ?Term) is det.
% 
atom_term([],[]).
atom_term(Atom,Term) :-
        (var(Atom) ->
         Term =.. [Functor|Arguments],
         (Arguments = [] ->
          Atom = Functor
         ;
          atom_concat(Functor,'(',FunctorBracket),
          atom_term(FunctorBracket,Arguments,Atom0),
          atom_concat(Atom0,')',Atom)
         )
        ;
         atom_concat(Atom,'.',AtomDot),
         atom_codes(AtomDot,AtomCode),
         read_from_codes(AtomCode,Term)
        ).
atom_term(Atom,Atom,[]).
atom_term(AtomIn,[Argument],AtomOut) :-
        (number(Argument) ->
         atom_number(ArgumentAtom,Argument)
        ;
         ArgumentAtom = Argument
        ),
        atom_concat(AtomIn,ArgumentAtom,AtomOut).
atom_term(AtomIn,[Argument|Arguments],AtomOut) :-
        (number(Argument) ->
         atom_number(ArgumentAtom,Argument)
        ;
         ArgumentAtom = Argument
        ),
        atom_concat(AtomIn,ArgumentAtom,Atom0),
        atom_concat(Atom0,',',Atom),
        atom_term(Atom,Arguments,AtomOut).
/*------------------------- End: Utility -------------------------*/
