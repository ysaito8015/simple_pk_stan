ipdb.r <- function()
{
    input <- ""
    while(!input %in% c("c","cont","continue"))
    {
        cat("ipdb.r>")
        # stdin connection to work outside interactive session
        input <- readLines("stdin",n=1)
        if(!input %in% c("c","cont","continue","exit"))
        {
            # parent.frame() runs command outside function environment
            print(eval(parse(text=input),parent.frame()))
        }else if(input=="exit")
        {
            stop("Exiting from ipdb.r...")
        }
    }
}
