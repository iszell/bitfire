all: gendirs acme drivecodes residents sectorwriters drivecode-fsdv installers bitnax d64write1 test-loaddecomp test-savetest test-spdtst

gendirs:
	@mkdir gen-binaries
	@mkdir gen-includes

acme:
	@$(MAKE) -C utils/acme-src/src

drivecodes:
	@$(MAKE) -C drive

residents:
	@$(MAKE) -C resident

sectorwriters:
	@$(MAKE) -C sectorwriter

drivecode-fsdv:
	@$(MAKE) -C drive fsdv

installers:
	@$(MAKE) -C installer

bitnax:
	@$(MAKE) -C utils/bitnax-src

d64write1:
	@$(MAKE) -C utils/d64write1-src

test-loaddecomp:
	@$(MAKE) -C teststuffs/loaddecomp

test-savetest:
	@$(MAKE) -C teststuffs/savetest

test-spdtst:
	@$(MAKE) -C teststuffs/speedmeter

clean:
	@rm -rf gen-includes gen-binaries
	@$(MAKE) -C utils/acme-src/src clean
	@$(MAKE) -C drive clean
	@$(MAKE) -C resident clean
	@$(MAKE) -C sectorwriter clean
	@$(MAKE) -C installer clean
	@$(MAKE) -C utils/bitnax-src clean
	@$(MAKE) -C utils/d64write1-src clean
	@$(MAKE) -C teststuffs/loaddecomp clean
	@$(MAKE) -C teststuffs/savetest clean
	@$(MAKE) -C teststuffs/speedmeter clean
