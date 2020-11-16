redPlayerTurn(Board, NewBoard) :-
      write('\nRed player turn\n\n'),
      moveFish(Board , Board1),
      placeRock(Board1, NewBoard),
      printBoard(NewBoard).


yellowPlayerTurn(NewBoard, FinalBoard) :-
      write('\nYellow player turn\n\n'),
      
      moveFish(Board , Board1),
      placeRock(Board1, FinalBoard),
      
      printBoard(FinalBoard).



gameLoop(Board, Player1, Player2) :-
      redPlayerTurn(Board, NewBoard),
      (
            (checkGameState('black', NewBoard), write('\nThanks for playing!\n'));
            (yellowPlayerTurn(NewBoard, FinalBoard),
                  (
                        (checkGameState('white', FinalBoard), write('\nThanks for playing!\n'));
                        (gameLoop(FinalBoard, Player1, Player2))
                  )
            )
      ).