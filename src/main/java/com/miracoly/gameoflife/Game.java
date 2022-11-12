package com.miracoly.gameoflife;

import java.util.Collection;
import java.util.List;
import java.util.stream.Stream;

/**
 * Solution inspired by rst0git's haskell solution.
 * Repository: <a href="https://github.com/rst0git/Game-of-Life-Haskell/blob/master/game-of-life.hs">...</a>
 */
public record Game(List<Position> board) {
    public Game nextTick() {
        return new Game(Stream.concat(getSurvivors().stream(), getBirths().stream()).toList());
    }

    private List<Position> getSurvivors() {
        return board.stream()
                .filter(pos -> List.of(2, 3).contains(countLiveNeighbors(pos)))
                .toList();
    }

    public List<Position> getBirths() {
        return board.stream()
                .map(this::getNeighbors)
                .flatMap(Collection::stream)
                .distinct()
                .filter(this::isEmpty)
                .filter(pos -> countLiveNeighbors(pos) == 3)
                .toList();
    }

    private boolean isAlive(Position pos) {
        return board.contains(pos);
    }

    private boolean isEmpty(Position pos) {
        return !isAlive(pos);
    }

    private int countLiveNeighbors(Position pos) {
        return Math.toIntExact(getNeighbors(pos).stream()
                .filter(this::isAlive)
                .count());
    }

    private List<Position> getNeighbors(Position pos) {
        return List.of(
                new Position(pos.x() - 1, pos.y() - 1),
                new Position(pos.x(), pos.y() - 1),
                new Position(pos.x() + 1, pos.y() - 1),
                new Position(pos.x() - 1, pos.y()),
                new Position(pos.x() + 1, pos.y()),
                new Position(pos.x() - 1, pos.y() + 1),
                new Position(pos.x(), pos.y() + 1),
                new Position(pos.x() + 1, pos.y() + 1)
        );
    }

}
