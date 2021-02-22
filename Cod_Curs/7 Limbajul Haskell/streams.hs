ones           = 1 : ones

naturalsFrom n = n : (naturalsFrom (n + 1))
naturals1      = naturalsFrom 0
naturals2      = 0 : (zipWith (+) ones naturals2)

evenNaturals1  = filter even naturals1
evenNaturals2  = zipWith (+) naturals1 naturals2

fibo           = 0 : 1 :
                 (zipWith (+) fibo (tail fibo))