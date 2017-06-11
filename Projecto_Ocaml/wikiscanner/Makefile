OCAMLBUILD=ocamlbuild -use-ocamlfind

OCAMLGRAPH=-I +ocamlgraph
GRAPH=graph.cmxa

OCAMLOPT=ocamlopt.opt
OCAMLLEX=ocamllex.opt

BIN=wikiscanner
GEN=$(BIN).ml $(BIN).cmi $(BIN).cmx $(BIN).o

all: $(BIN)

$(BIN): $(BIN).mll
	$(OCAMLBUILD) $(BIN).native
	@mv $(BIN).native $(BIN)

clean::
	ocamlbuild -clean

.PHONY: clean classic

classic: $(BIN).cmx
	$(OCAMLOPT) -o $(BIN) $(OCAMLGRAPH) $(GRAPH) $<

clean::
	rm -f *~ *# $(GEN) $(BIN)

# generic rules
###############

%.ml: %.mll
	$(OCAMLLEX) $<

%.cmx: %.ml
	$(OCAMLOPT) -c $(OCAMLGRAPH) $<

# doc
#####

$(BIN).pdf: $(BIN).mll
	ocamlweb wikiscanner.mll -o $(BIN).tex
	rubber -d $(BIN).tex && evince $@

clean::
	rm -f *.pdf *.aux *.tex *.log *.aux
