/* -*- Mode:Prolog; coding:ISO-8859-1; -*- */


:- module(z_modulename_gdlAi,[z_gdlAi/5]).
:- use_module(library(lists)).

/* -------------
AiWorld = [gdl-X,       % in GDL-I alle Fakten zum aktuellen Zustand; GDL-II die sees-Fakten
           allowed-X,   % die erlaubten Aktionen im aktuellen Zustand
           player-X]    % welcher Spieler die KI ist
------------- */

z_gdlAi(_Version,AiWorld,KbIn,AiAction,KbOut) :-
        worldget(gdlPath,AiWorld,GdlPath),
        use_module(GdlPath),
        gdlAi(AiWorld,KbIn,AiAction,KbOut).

gdlAi(AiWorld,KbIn,AiAction,KbOut) :-
        worldget(player,AiWorld,Player),
        worldget(gdl,AiWorld,Gdl),
        append([KbIn,Gdl],KbOut),
        worldget(allowed,AiWorld,AllowedActions),
        AllowedActions = [AiAction|_],
        
        write(Player-'Knowledgebase:'-KbOut),
        nl.


worldget(Key,List,Value) :-
        member(Key-Value,List).
worldput(Key,List,NewValue,[Key-NewValue|Rest]) :-
        (member(Key-_,List) -> select(Key-_,List,Rest) ; List = Rest).