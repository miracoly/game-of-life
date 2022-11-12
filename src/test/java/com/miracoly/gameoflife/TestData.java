package com.miracoly.gameoflife;

import java.util.List;
import java.util.function.Supplier;

public class TestData {

    private TestData() {
    }


    /*
        x - - x -
        x - - x -
        - - - - -
        - - x - -
        - x - - -
     */
    public static final Supplier<Game> INITIAL_GAME_UNDERPOPULATION_ALL_DIE = () -> new Game(List.of(
            new Position(1, 1),
            new Position(4, 1),
            new Position(1, 2),
            new Position(4, 2),
            new Position(3, 4),
            new Position(2, 5)
    ));

    /*
    - - - - -
    - x x - -
    x - - x -
    - x x - -
    - - - - -
    */
    public static final Supplier<Game> INITIAL_STATIC_GAME = () -> new Game(List.of(
            new Position(2, 2),
            new Position(3, 2),
            new Position(1, 3),
            new Position(4, 3),
            new Position(2, 4),
            new Position(3, 4)
    ));

    /*
    - - - - -
    - - x - -
    - x x x -
    - - x - -
    - - - - -
    */
    public static final Supplier<Game> INITIAL_GAME_OVERPOPULATION = () -> new Game(List.of(
            new Position(3, 2),
            new Position(2, 3),
            new Position(3, 3),
            new Position(4, 3),
            new Position(3, 4)
    ));

    /*
    - - - - -
    - x x x -
    - x - x -
    - x x x -
    - - - - -
    */
    public static final Supplier<Game> NEXT_TICK_OVERPOPULATION = () -> new Game(List.of(
            new Position(3, 2),
            new Position(2, 3),
            new Position(4, 3),
            new Position(3, 4),
            new Position(2, 2),
            new Position(4, 2),
            new Position(2, 4),
            new Position(4, 4)
    ));

    /*
    - - - - -
    - - x - -
    - x - - -
    - - - x -
    - - - - -
    */
    public static final Supplier<Game> INITIAL_GAME_BIRTH = () -> new Game(List.of(
            new Position(3, 2),
            new Position(2, 3),
            new Position(4, 4)
    ));

    /*
    - - - - -
    - - - - -
    - - x - -
    - - - - -
    - - - - -
    */
    public static final Supplier<Game> NEXT_TICK_BIRTH = () -> new Game(List.of(
            new Position(3, 3)
    ));

}
