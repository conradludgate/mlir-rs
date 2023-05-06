# MLIR pass

MLIR first loads up a PassManager with passes. It then loads all the "dependant dialects" from these passes.

It then creates an analysis manager over the module operation - analysis manager is a caching store for analysis performed on operations I think.

It then calls `runPasses` which forwards directly into `OpToOpPassAdaptor::runPipeline`. This is a glorified `passes.for_each(run)`.

`OpToOpPassAdaptor::run` seems to recursively apply passes to every operation. It makes use of `am.nest(op)` to ensure the analysis context is stacked properly
