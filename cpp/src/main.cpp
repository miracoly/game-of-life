#include <cerrno>
#include <chrono>
#include <csignal>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <limits>
#include <optional>
#include <thread>

#include "gol/Game.hpp"

namespace {

volatile std::sig_atomic_t gShutdownRequested = 0;

void HandleSigint(int) {
  gShutdownRequested = 1;
}

std::optional<std::uint32_t> parseSeed(int argc, char* argv[]) {
  if (argc < 2) return std::nullopt;

  char* end = nullptr;
  errno = 0;
  unsigned long parsed = std::strtoul(argv[1], &end, 10);
  if (errno != 0 || !end || *end != '\0' ||
      parsed > std::numeric_limits<std::uint32_t>::max()) {
    std::fprintf(stderr, "Invalid seed '%s'\n", argv[1]);
    std::exit(EXIT_FAILURE);
  }
  return static_cast<std::uint32_t>(parsed);
}

}  // namespace

int main(int argc, char* argv[]) {
  for (int i = 0; i < argc; ++i) {
    std::cout << i << ": " << argv[0] << '\n';
  }
  gol::Game game{20, 10};

  const auto seed = parseSeed(argc, argv);
  const std::uint32_t seed_used = game.Seed(seed);
  std::printf("Seed: %u\n", seed_used);

  gol::Game::InitRender();
  std::signal(SIGINT, HandleSigint);

  while (!gShutdownRequested) {
    game.Render();
    game.Step();
    std::this_thread::sleep_for(std::chrono::seconds(1));
  }

  gol::Game::CleanupRender();
  return EXIT_SUCCESS;
}
