                                                                    **********************
                                                                    *****   README   *****
                                                                    **********************

*****************************************************************************************************************************************************************************

PLEASE, BE SURE TO READ THIS FILE BEFORE CHECKING SOURCE CODE OR TRYING TO COMPILE

WHAT IS CURRENTLY IMPLEMENTED :
  -FORD FULKERSON ALGORITHM
  -RANDOM GRAPH GENERATOR
  -MULTI-SOURCE MULTI-SINK FORD FULKERSON ALGORITHM (BUT VIRTUAL NODES ARE NOT ERASED IN THE END)

*****************************************************************************************************************************************************************************

THE FIRST THING YOU SHOULD DO IS GENERATING A RANDOM GRAPH USING THE MAKE RANDOM INSRUCTION (ALL DETAILS ARE IN THE MAKEFILE)
  ->MAKE RANDOM S=X GENERATES A RANDOM GRAPH HAVING X NODES, NODE 0 BEEING SRC AND NODE X-1 BEEING DST (YOU CAN SEE THE GENERATED GRAPH FILE CALLED "RANDOM" IN THE GRAPH FOLDER)

ONCE YOU HAVE GENERATED A GRAPH YOU CAN TRY TO RUN THE FF ALGORITHM ON IT USING MAKE FTEST
  ->THIS WILL OUTPUT AN IMAGE OF THE RESULT GRAPH AFTER EXECUTING FF ON IT CALLED FF_IMG IN THE ROOT FOLDER OF THE PROJECT

  ===> NOTE THAT YOU CAN ALSO QUICKLY GENERATE A GRAPH AND EXECUTE THE FF ALGORITHM ON IT USING MAKE RUN S=X, X BEEING THE NUMBER OF NODES WANTED IN THE GRAPH <===

*****************************************************************************************************************************************************************************

YOU CAN ALSO TRY THE MULTI FF ALGORITHM BY USING THE MAKE MULTI INSTRUCTION
  ===> THIS ONE IS A BIT MORE COMPLEX
    ->MAKE MULTI S=X D=Y EXECUTES THE ALGORITHM ON A GRAPH ON WHICH YOU CHOOSE TO HAVE X SOURCES AND Y SINKS
      ===>!!! NOTE THAT YOU HAVE TO MANUALLY PRECISE THE SOURCE IDS FOLLOWED BY THE SINKS IDS IN THE MAKEFILE (END OF LINE 47) !!!<===

===> KEEP IN MIND THAT YOU CAN ALSO PRECISE AN OTHER GRAPH FILE (THAT MUST BE PUT IN THE GRAPH FOLDER) WHEN EXECUTING FF ALGORITHMS USING MAKE F=NAME S=SOURCE D=SINK <===

                                          ===> BE CAREFUL WITH S AND D, IN MULTI MODE THAY HAVE A DIFFERENT MEANING <====


*****************************************************************************************************************************************************************************
*****************************************************************************************************************************************************************************
*****************************************************************************************************************************************************************************

                               THANKS FOR READING, NOW YOU CAN ENJOY THE OUTSTANDING SKILLS OF THE PROGRAMMERS BEHIND THIS PROJECT !



                                                                          COPYRIGHTS :

                                                                           GAILLARD
                                                                               &
                                                                           CAZAUBON
                                                                             2019
                                                                   "AWESOMENESS IS OUR CREED"
                                                                      ALL RIGHTS RESERVED


*****************************************************************************************************************************************************************************
*****************************************************************************************************************************************************************************
*****************************************************************************************************************************************************************************
