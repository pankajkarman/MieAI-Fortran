# Compiler and flags
FC=gfortran
FFLAGS=-fcoarray=single

# Directories
SRC_DIR = FKB
BIN_DIR = bin
BUILD_DIR = build/obj
TESTS_DIR = src
UTILS_DIR = utils
INCLUDE_DIR = build/include
LIB_DIR = build/lib

# Source files
SOURCES = $(SRC_DIR)/mod_kinds.F90 $(SRC_DIR)/mod_activation.F90 $(SRC_DIR)/mod_layer.F90 $(SRC_DIR)/mod_random.F90 $(SRC_DIR)/mod_dense_layer.F90 $(SRC_DIR)/mod_dropout_layer.F90 $(SRC_DIR)/mod_batchnorm_layer.F90 $(SRC_DIR)/mod_io.F90 $(SRC_DIR)/mod_parallel.F90 $(SRC_DIR)/mod_network.F90 $(SRC_DIR)/mod_transformer.F90

#SOURCES = $(wildcard $(SRC_DIR)/*.F90)
OBJECTS = $(patsubst $(SRC_DIR)/%.F90,$(BUILD_DIR)/%.o,$(SOURCES))

# Test Sources and executable
TEST_SOURCES = $(wildcard $(TESTS_DIR)/*.F90)
TEST_EXECUTABLES = $(patsubst $(TESTS_DIR)/%.F90,$(BIN_DIR)/%,$(TEST_SOURCES))

# Build FKB library
.PHONY: all clean

all: link tests

link: $(LIB_DIR)/libneural.a 

$(LIB_DIR)/libneural.a: $(OBJECTS)
	ar r $@ $^

$(LIB_DIR)/libneural.so: $(OBJECTS)
	$(FC) -shared $@ $^

# Build rule for each object file
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.F90
	$(FC) $(FFLAGS) -J$(INCLUDE_DIR) -o $@ -c $<
    
# Build test executables
tests: $(TEST_EXECUTABLES)

$(BIN_DIR)/%: $(TESTS_DIR)/%.F90
	$(FC) $(FFLAGS) -o $@ $^ -I$(INCLUDE_DIR) -L$(LIB_DIR) -lneural

clean:
	rm -rf $(BIN_DIR)/* $(BUILD_DIR)/*.o $(LIB_DIR)/*.a $(LIB_DIR)/*.so $(INCLUDE_DIR)/*.mod
