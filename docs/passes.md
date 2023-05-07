# MLIR pass

MLIR first loads up a PassManager with passes. It then loads all the "dependant dialects" from these passes.

It then creates an analysis manager over the module operation - analysis manager is a caching store for analysis performed on operations I think.

It then calls `runPasses` which forwards directly into `OpToOpPassAdaptor::runPipeline`. This is a glorified `passes.for_each(run)`.

`OpToOpPassAdaptor::run` seems to recursively apply passes to every operation. It makes use of `am.nest(op)` to ensure the analysis context is stacked properly


# ConversionPatterns

You can make a pass that applies partial conversions (lowerings).

Give it a list of legal dialects, the illegal dialect and the matchAndRewrite patterns to convert. Some operations in a dialect can be conditionally legal.

The partial conversion walks the operation tree in pre-order and collects every operation that is illegal into a list. It then iterates the list to apply the conversions.
Conversions come in 2 forms. Patterns and Folds.

Patterns store the RewritePattern by operation name. There are also op-agnostic RewritePatterns stored in a list. Patterns have a "benefit" and sorted by that.
After sorting, apply patterns in order and exit on first success.
