#include <chrono>
#include <csignal>
#include <cstdio>
#include <cstdlib>
#include <thread>
#include "gol/Game.hpp"

namespace {}

int main() {
  gol::Game game{20, 10};
  game.Seed();

  gol::Game::InitRender();

  while (true) {
    game.Render();
    // game.Step();
    std::this_thread::sleep_for(std::chrono::seconds(1));
  }

  gol::Game::CleanupRender();

  return 0;
}
