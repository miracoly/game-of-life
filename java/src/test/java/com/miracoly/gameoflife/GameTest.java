package com.miracoly.gameoflife;

import org.junit.jupiter.api.DisplayNameGeneration;
import org.junit.jupiter.api.DisplayNameGenerator;
import org.junit.jupiter.api.Test;

import static java.util.Collections.emptyList;
import static org.assertj.core.api.Assertions.assertThat;

@DisplayNameGeneration(DisplayNameGenerator.ReplaceUnderscores.class)
class GameTest {

    @Test
    void next_tick_all_die() {
        final var nextTick = TestData.INITIAL_GAME_UNDERPOPULATION_ALL_DIE.get().nextTick();

        assertThat(nextTick).isEqualTo(new Game(emptyList()));
    }

    @Test
    void next_tick_static_game() {
        final var nextTick = TestData.INITIAL_STATIC_GAME.get().nextTick();

        assertThat(nextTick).isEqualTo(TestData.INITIAL_STATIC_GAME.get());
    }

    @Test
    void next_tick_overpopulation() {
        final var nextTick = TestData.INITIAL_GAME_OVERPOPULATION.get().nextTick();

        assertThat(nextTick).isEqualTo(TestData.NEXT_TICK_OVERPOPULATION.get());
    }

    @Test
    void next_tick_birth() {
        final var nextTick = TestData.INITIAL_GAME_BIRTH.get().nextTick();

        assertThat(nextTick).isEqualTo(TestData.NEXT_TICK_BIRTH.get());
    }

    @Test
    void get_births() {
        final var births = TestData.INITIAL_GAME_BIRTH.get().getBirths();

        assertThat(births).isEqualTo(TestData.NEXT_TICK_BIRTH.get().board());
    }

}