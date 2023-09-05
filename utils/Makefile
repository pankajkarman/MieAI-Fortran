# Compiler and flags
FC := gfortran

BUILD_DIR = ../build/obj
INCLUDE_DIR = ../build/include

.PHONY: all clean

all:
	$(FC) -c -o $(BUILD_DIR)/QuantileTransformer.o QuantileTransformer.F90 -J$(INCLUDE_DIR)
	$(FC) -c -o $(BUILD_DIR)/scaler.o scaler.F90 -J$(INCLUDE_DIR)
	$(FC) -o test_quantile -I$(INCLUDE_DIR) test_quantile.F90 $(BUILD_DIR)/QuantileTransformer.o
	$(FC) -o test_scaler -I$(INCLUDE_DIR) test_scaler.F90 $(BUILD_DIR)/scaler.o

clean:
	rm -rf *.o *.mod test_quantile test_scaler
