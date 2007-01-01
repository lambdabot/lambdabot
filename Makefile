clean:
	./Setup.hs clean
	rm -f ${CLEANS}
	rm -f *~
    
CLEANS= BotPP bf djinn ft hoogle lambdabot unlambda  \
        L.hi ShowFun.hi ShowQ.hi SmallCheck.hi \
        L.o  ShowFun.o  ShowQ.o  SmallCheck.o  \
        config.h config.log config.status      
