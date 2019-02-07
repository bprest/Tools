
# https://stackoverflow.com/questions/8093914/use-trycatch-skip-to-next-value-of-loop-upon-error
i = 1

while (i<100) {
  draw = runif(1)
  possibleError <- tryCatch({
    if (draw<0.05) runif() else print(i)
    },
    error = function(e) e)
  if (inherits(possibleError, "error")) message('Error on ',i,'. Retrying...')
  if (!inherits(possibleError, "error")) i = i+1
}