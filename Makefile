# sml-obj build
#
#   make            build the test binary with MLton (default)
#   make test       build + run tests under MLton
#   make test-poly  run tests under Poly/ML (use-and-run; no link step)
#   make all-tests  run the suite under both compilers
#   make fixtures   regenerate test/fixtures.sml (needs python3)
#   make clean      remove build artifacts
#
# Layout B (dependent): own sources live in src/; sml-glm is vendored under lib/
# and loaded first.

MLTON      ?= mlton
POLY       ?= poly
BIN        := bin
GLMDIR     := lib/github.com/sjqtentacles/sml-glm
TEST_MLB   := test/test.mlb
SRCS       := $(wildcard $(GLMDIR)/* src/* test/*.sml) $(TEST_MLB)

.PHONY: all test poly test-poly all-tests fixtures example clean

all: $(BIN)/test-mlton

example: $(BIN)/wireframe
	mkdir -p assets
	./$(BIN)/wireframe

$(BIN)/wireframe: $(SRCS) examples/wireframe.sml examples/sources.mlb | $(BIN)
	$(MLTON) -output $@ examples/sources.mlb

$(BIN)/test-mlton: $(SRCS) | $(BIN)
	$(MLTON) -output $@ $(TEST_MLB)

test: $(BIN)/test-mlton
	$(BIN)/test-mlton

# Poly/ML has no native .mlb support; the suite runs at top level and exits on
# its own. Load the vendored sml-glm first, then mesh, then the test driver.
poly test-poly:
	printf 'use "$(GLMDIR)/glm.sig";\nuse "$(GLMDIR)/glm.sml";\nuse "src/mesh.sig";\nuse "src/mesh.sml";\nuse "test/harness.sml";\nuse "test/fixtures.sml";\nuse "test/support.sml";\nuse "test/test_obj.sml";\nuse "test/test_mtl.sml";\nuse "test/test_ply.sml";\nuse "test/test_buffers.sml";\nuse "test/test_edge.sml";\nuse "test/entry.sml";\nuse "test/main.sml";\n' | $(POLY) -q --error-exit

all-tests: test test-poly

fixtures:
	python3 bin/gen_fixtures.py

$(BIN):
	mkdir -p $(BIN)

clean:
	rm -f $(BIN)/test-mlton
