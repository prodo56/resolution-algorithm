import ply.lex as lex
import re

# token names
tokens = (
    'PREDICATE',
    'VARIABLE',
    'AND',
    'OR',
    'NOT',
    'IMPLIES',
    'LPAREN',
    'RPAREN',
    'CONSTANT'
)

# regex for tokens lex
t_AND = r'\&'
t_OR = r'\|'
t_NOT = r'\~'
t_VARIABLE = r'[a-z0-9]+'
t_PREDICATE = r'[A-Za-z]+\(.*?\)'
t_IMPLIES = r'=>'
t_LPAREN = r'\( '
t_RPAREN = r' \)'
t_CONSTANT = r'\w+'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


# A string containing ignored characters (spaces and tabs)
t_ignore = ' \t'


# Error handling
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

#parsed sentence dict
parseDict={}
#predicateDict{predicate:line_number list}
predicateDict = {}
#variableDic{variable:predicate list}
variableDict = {}
#constantDict{Constant:appearance_list}
constantDict = {}

KBDict = {}
names = {}
cnfKB = {}

def parseSentence(inputString,lineno):
    #tokens for the given input string without variables and constant
    tokensList=[]
    lexer.input(inputString)
    # Tokenize
    notPresent = False
    while True:
        tok = lexer.token()
        predicateArguments = []
        if not tok:
            break  # No more input
        #print (tok.type, tok.value, tok.lineno, tok.lexpos)
        tokensList.append((tok.type, tok.value, lineno, tok.lexpos))
        if tok.type == 'NOT':
            notPresent = True
        elif tok.type == 'PREDICATE':
            if notPresent:
                tok.value = "~"+tok.value
                notPresent = False
            if tok.value in predicateDict:
                if lineno not in predicateDict[tok.value]:
                    l = predicateDict[tok.value]
                    l.append(lineno)
                    predicateDict[tok.value] = l
            else:
                predicateDict[tok.value] = [lineno]

    #symbols = inputString.split(" ")
    #print symbols, predicateDict

    for x in predicateDict.keys():
        #m = re.search('(?<=abc)def', 'abcdef')
        variables=x[x.find('(')+1:x.find(')')]
        variableList=variables.split(',')
        for v in variableList:
            lexer.input(v)
            tok = lexer.token()
            #print (tok.type, tok.value, tok.lineno, tok.lexpos)
            #if (tok.type, tok.value, lineno, tok.lexpos) not in tokensList:
            #    tokensList.append((tok.type, tok.value, lineno, tok.lexpos))
            if tok.type =='VARIABLE':
                if tok.value in variableDict:
                    if tok.lineno not in variableDict[tok.value]:
                        l = variableDict[tok.value]
                        l.append(x)
                        variableDict[tok.value] = l
                else:
                    variableDict[tok.value] = [x]
            elif tok.type == 'CONSTANT':
                if tok.value in constantDict:
                    if tok.lineno not in constantDict[tok.value]:
                        l = constantDict[tok.value]
                        l.append(x)
                        constantDict[tok.value] = l
                else:
                    constantDict[tok.value] = [x]
    return tokensList


def moveNot(lexLogic):
    print lexLogic
    result = []
    i = 0
    while i<len(lexLogic):
        (elementType, elementValue, lineno, lexpos) = lexLogic[i]
        if elementType == 'NOT':
            result.append(elementValue)
        elif elementType == 'LPAREN':
            if result and result[-1] == "~":
                j = i+1
                result.pop()
                result.append("(")
                while j<len(lexLogic):
                    (elementType1, elementValue1, lineno1, lexpos1) = lexLogic[j]
                    if elementType1 == 'PREDICATE' and lexLogic[j-1][0] != 'NOT':
                        result.append("~")
                        result.append(elementValue1)
                    elif elementType1 == 'PREDICATE' and lexLogic[j-1][0] == 'NOT':
                        result.append(elementValue1)
                    elif elementType1 == "OR":
                        result.append("&")
                    elif elementType1 == "AND":
                        result.append("|")
                    elif elementType1 == 'RPAREN':
                        result.append(")")
                        break
                    j = j+1
                i = j
            else:
                result.append(elementValue)
        else:
                #elementType1 == 'PREDICATE':
            result.append(elementValue)
        i = i + 1
    sentence = " ".join(result)
    return parseSentence(sentence,0)





def moveNotInwards(stack):
    result=[]
    tempStack = list(reversed(stack))
    lexLogic=parseSentence(" ".join(tempStack),0)
    i=0
    while i<len(lexLogic):
        (elementType, elementValue, lineno, lexpos) = lexLogic[i]
        if elementType == 'OR':
            result.append("&")
        elif elementType == 'AND':
            result.append("|")
        elif elementType == 'PREDICATE':
            if lexLogic[i-1][0] == 'NOT':
                result.append(elementValue)
            else:
                result.append("~")
                result.append(elementValue)
        elif elementType == 'LPAREN' or elementType == 'RPAREN':
            result.append(elementValue)
        i = i+1
    #print list(reversed(result))
    return result


def removeParanthesis(stack):
    result=[]
    lexLogic = parseSentence(" ".join(stack), 0)
    i=0
    while(i<len(lexLogic)):
        (elementType, elementValue, lineno, lexpos) = lexLogic[i]
        if elementType == 'OR':
            result.append(elementValue)
        elif elementType == 'AND':
            result.append(elementValue)
        elif elementType == 'PREDICATE':
            result.append(elementValue)
        elif elementType == 'NOT':
            result.append(elementValue)
        i = i+1
    return result

def removeImplicationinLogic(lexLogic):
    lexLogic = moveNot(lexLogic)
    stack=[]
    result=[]
    i=0
    while(i<len(lexLogic)):
        (elementType, elementValue, lineno, lexpos) = lexLogic[i]
        if elementType != 'IMPLIES':
            stack.append((elementValue, elementType))
            result.append(elementValue)
            i = i + 1
        else:
            j = len(result)-1
            stackresult = []
            if result[j] == ')':
                while(result[j]!='('):
                    j = j-1
                    stackresult.append(result.pop())
                    #print len(result)
                stackresult.append(result.pop())
                stackresult=moveNotInwards(stackresult)
                #result.append("~")
                #result = result + list(reversed(stackresult))
                result = result + stackresult
                result.append("|")
            else:
                stackresult.append(result.pop())
                result.append("~")
                result = result + list(reversed(stackresult))
                result.append("|")
            i = i + 1
    if "&" not in result:
        result = removeParanthesis(result)
    return " ".join(result)

def removeMultipleNot(lexLogic, logic):
    #substringCount = re.match("(\(~){2,}", logic).group(0).count("~")
    patternIndex = [(m.start(0), m.end(0)) for m in re.finditer("(\(~){2,}", logic)]
    for (start,end) in patternIndex:
        temp=""
        if logic[:start] != "":
            temp=logic[:start]
        parsedrighthalf=parseSentence(logic[end:], 0)
        #print parsedrighthalf
        notCount = logic[start:end].count("~")
        if notCount%2==0:
            notNecessary=False
        else:
            notNecessary=True
        righthalf=[]
        count = notCount
        for (elementType, elementValue, lineno, lexpos) in parsedrighthalf:
            if elementType == 'RPAREN' and notCount > 0:
                notCount = notCount -1
            elif elementType == 'PREDICATE' and count%2 !=0:
                righthalf.append(elementValue)
                righthalf.append(")")
            else:
                righthalf.append(elementValue)
        if notNecessary:
            temp = temp + "(~"+"".join(righthalf)
        else:
            temp = temp + " ".join(righthalf)
        logic = temp
    return logic

def infixToPostfix(expr):
    prec = {}

    prec[")"] = 1
    prec["~"] = 3
    prec["&"] = 2
    prec["|"] = 2
    #prec["=>"] = 1
    prec["("] = 1
    opStack = []
    postfixList = []
    tokenList = parseSentence(expr,0)
    newlist=[]
    newTokenList=[]
    for (elementType, elementValue, lineno, lexpos) in tokenList:
        newTokenList.append(elementValue)
    #print " ".join(newTokenList)
    for token in newTokenList:
        if token not in ["|", "&", "(", ")"]:
            postfixList.append(token)
        elif token == '(':
            opStack.append(token)
            #postfixList.append(token)
        elif token == ')':
            topToken = opStack.pop()
            while topToken != '(':
                postfixList.append(topToken)
                if opStack:
                    topToken = opStack.pop()
            #postfixList.append(")")
        else:
            while (len(opStack)!= 0) and \
               (prec[opStack[-1]] >= prec[token]):
                  postfixList.append(opStack.pop())
            opStack.append(token)

    while (len(opStack)!= 0):
        postfixList.append(opStack.pop())
    #print " ".join(postfixList)
    return " ".join(postfixList)

def cnfConversion(stack):
    result = []
    newList=[]
    lexlogic = parseSentence(stack,0)
    i = 0
    while (i < len(lexlogic)):
        (elementType, elementValue, lineno, lexpos) = lexlogic[i]
        if elementType =='NOT':
            i = i+1
            (elementType1, elementValue1, lineno1, lexpos1) = lexlogic[i]
            newList.append("~"+elementValue1)
        else:
            newList.append(elementValue)
        i = i+1

    for elementValue in newList:
        if elementValue == "&":
            tempList = result[-1]
            result.pop()
            for t in tempList:
                result[-1].append(t)
        elif elementValue == "|":
            tempList = result[-1]
            result.pop()
            tempList1 = result[-1]
            result.pop()
            result.append([])
            for t in tempList:
                for i in tempList1:
                    result[len(result) - 1].append(t + '|' + i)
        else:
            result.append([elementValue])
    return result[0]

def clearallDict():
    parseDict.clear()
    # predicateDict{predicate:line_number list}
    predicateDict.clear()
    # variableDic{variable:predicate list}
    variableDict.clear()
    # constantDict{Constant:appearance_list}
    constantDict.clear()

newKB = []

finalKB=[]
for i in xrange(len(newKB)):
    #print newKB[i]
    finalKB.append(parseSentence(newKB[i],i+1))

predicateDict1 = {}
constantDict1 = {}
variableDict1 = {}
parseDict1 = {}

def copyList(line):
    result=[]
    for element in line:
        result.append(element)
    return result

def makeCopiesofDict():
    global predicateDict1
    global constantDict1
    global variableDict1
    global parseDict1
    for key in predicateDict.keys():
        predicateDict1[key] = copyList(predicateDict[key])
    for key in constantDict.keys():
        constantDict1[key] = copyList(constantDict[key])
    for key in variableDict.keys():
        variableDict1[key] = copyList(variableDict[key])
    for key in parseDict.keys():
        parseDict1[key] = copyList(parseDict[key])



def getPredicateName(predicate):
    return predicate.split("(")[0]

def allPredicateNameDict():
    global names
    #print predicateDict1
    for predicate in predicateDict1.keys():
        names[getPredicateName(predicate)]=predicateDict1[predicate]

truePredicateDict = {}

'''
def unify(logic1, logic2, substitution):
    if substitution == None:
        return None
    if logic1 == logic2:
        return logic1
'''
def isPresentInKB(query):
    for key in truePredicateDict.keys():
        if truePredicateDict[key] == query:
            return True
    return False



def parseArgumentsfromPredicate(predicateArguments):
    arguments=[]
    for v in predicateArguments:
        lexer.input(v)
        while True:
            tok = lexer.token()
            if not tok:
                break  # No more input
            arguments.append((tok.type, tok.value, tok.lexpos))
    return arguments

def getArgumentList(predicate):
    return (predicate[predicate.find('(') + 1:predicate.find(')')]).split(',')


def unify(predicateName, predicateArgs, unificationDic):
    result=[]
    predicateArguments = parseArgumentsfromPredicate(predicateArgs)
    keys=[]
    for k in unificationDic.keys():
        keys.append(k[1])
    for (elementType, elementValue, elementPos) in predicateArguments:
        if elementValue in keys and elementType == 'VARIABLE':
            result.append((unificationDic[(elementType, elementValue)][1]))
        elif elementType == 'CONSTANT':
            result.append((elementValue))
        else:
            result.append(elementValue)
    '''for (elementType, elementValue, elementPos) in unificationDic.keys():
        if elementType != 'CONSTANT':
            result.append(unificationDic[(elementType, elementValue, elementPos)][1])
        else:
            result.append(elementValue)'''
    return predicateName + "(" + ",".join(result) + ")"



resolutionKB=[]

def unifySentence(sentence,unificationValues):
    result = []
    for predicate in sentence.split("|"):
        argsPredicate = getArgumentList(predicate)
        unificationKeys=unificationValues.keys()
        #print unificationKeys
        keys=[]
        for a in unificationKeys:
            keys.append(a[1])
        #for args in argsPredicate:
        #    if args in keys:
        #        result.append(unify(getPredicateName(predicate), argsPredicate, unificationValues))
        #        break
        if set(keys) & set(argsPredicate):
            result.append(unify(getPredicateName(predicate), argsPredicate, unificationValues))
        else:
            result.append(predicate)
    return " | ".join(result)

loopstack=[]


def findNegatedPredicate(predicate,KB):
    result=[]
    if "~" in predicate:
        predicateToSearch = predicate.replace("~", "")
    else:
        predicateToSearch = "~" + predicate
    predicateToSearchName = getPredicateName(predicateToSearch)
    for key in KB.keys():
        sentence = KB[key]
        sentencePredicates=sentence.split("|")
        sentencePredicatesNames=[]
        for predicate in sentencePredicates:
            sentencePredicatesNames.append(getPredicateName(predicate.replace(" ", "")))
        if predicateToSearchName in sentencePredicatesNames:
            result.append(key)
    return result

def canUnify(predicate1,predicate2):
    predicate1Arguments = getArgumentList(predicate1)
    predicate2Arguments = getArgumentList(predicate2)
    predicate1parsedArguments = parseArgumentsfromPredicate(predicate1Arguments)
    predicate2parsedArguments = parseArgumentsfromPredicate(predicate2Arguments)
    i = 0
    while i<len(predicate2Arguments):
        (elementType, elementValue, elementPos) = predicate1parsedArguments[i]
        (elementType2, elementValue2, elementPos2) = predicate2parsedArguments[i]
        if elementType == 'CONSTANT' and elementType2 == 'CONSTANT' and elementValue != elementValue2:
            return False
        i = i+1
    return True


def unifyPredicates(predicate1, predicate2):
    result ={}
    predicate1Arguments = getArgumentList(predicate1)
    predicate2Arguments = getArgumentList(predicate2)
    predicate1parsedArguments = parseArgumentsfromPredicate(predicate1Arguments)
    predicate2parsedArguments = parseArgumentsfromPredicate(predicate2Arguments)
    i = 0
    while i < len(predicate2Arguments):
        (elementType, elementValue, elementPos) = predicate1parsedArguments[i]
        (elementType2, elementValue2, elementPos2) = predicate2parsedArguments[i]
        if elementType == 'CONSTANT' and elementType2 == 'VARIABLE':
            if (elementType2, elementValue2) in result.keys() and result[(elementType2, elementValue2)] != (elementType, elementValue):
                return False,{}
            elif (elementType2, elementValue2) not in result.keys():
                result[(elementType2, elementValue2)] = (elementType, elementValue)
        elif elementType == 'VARIABLE' and elementType2 == 'CONSTANT':
            if (elementType, elementValue) in result.keys() and result[(elementType, elementValue)] != (elementType2, elementValue2):
                return False,{}
            elif (elementType, elementValue) not in result.keys():
                result[(elementType, elementValue)] = (elementType2, elementValue2)
        elif elementType == 'VARIABLE' or elementType2 == 'VARIABLE':
                result[(elementType, elementValue)] = (elementType2, elementValue2)
        elif elementType == 'CONSTANT' and elementType2 == 'CONSTANT' and elementValue != elementValue2:
            return False,{}
        i = i + 1
    if set(result.keys()) & set(result.values()):
        intersectionlist = list(set(result.keys()) & set(result.values()))
        i = 0
        ##check if key in dict already present if present check the value if values are different then return false
        constantValue=()
        constantPresent = False
        while i<len(intersectionlist):
            (elementType, elementValue) = intersectionlist[i]
            if result[(elementType, elementValue)][0] == 'CONSTANT':
                constantValue=result[(elementType, elementValue)]
                constantPresent = True
                break
            i = i + 1
        if constantPresent:
            j = 0
            keyIntersection=[]
            for item in intersectionlist:
                for key in result.keys():
                    if result[key] == item:
                        keyIntersection.append(key)
            while j<len(keyIntersection):
                #(elementType, elementValue) =
               # if result[intersectionlist[j]][0]!='CONSTANT':
                if result[keyIntersection[j]][0] == 'CONSTANT' and result[keyIntersection[j]][1] != constantValue[1]:
                    return False,{}
                else:
                    result[keyIntersection[j]] = constantValue
                j = j + 1
        else:
            value = intersectionlist[0]
            j = 0
            while j < len(intersectionlist):
                result[intersectionlist[j]] == value
                j = j + 1
    return True, result


def checkTautology(sentence):
    sentencePredicates = sentence.replace(" ", "").split("|")
    i = 0
    while i<len(sentencePredicates):
        predicate1 = sentencePredicates[i]
        j = i+1
        while j<len(sentencePredicates):
            predicate2 = sentencePredicates[j]
            if "~" in predicate1:
                if predicate1 == "~"+predicate2:
                    return True
            elif "~" in predicate2:
                if predicate2 == "~"+predicate1:
                    return True
            j = j + 1
        i = i + 1
    return False

m = 1
resolve=[]
isResolved =False
index = 0
def resolution1(KB,resolvent):
    global isResolved
    global loopstack
    global index
    print KB
    global m
    #sentence1 = KB[KB.keys()[0]]

    sentence1Predicates = resolvent.replace(" ","").split("|")
    negatedPredicateSentences=[]
    for predicate in sentence1Predicates:
        negatedPredicateSentences.append(findNegatedPredicate(predicate,KB))
    if not negatedPredicateSentences:
       return
       #negatedPredicateSentences.sort(key=len)
    print "loopstack", loopstack
    i = 0
    while i<len(sentence1Predicates):
        predicate = sentence1Predicates[i]
        predicateName = getPredicateName(predicate)
        for negatedPredicateSentence in negatedPredicateSentences:
            print i, sentence1Predicates
            #if i>len(sentence1Predicates):
             #   return

            for sentence2LineNumber in negatedPredicateSentence:
                sentence2 = KB[sentence2LineNumber]
                sentence2Predicates = sentence2.replace(" ","").split("|")
               #sentence2predicate =  sentence2Predicates[j]
                for predicate2 in sentence2Predicates:
                    predicate2Name = getPredicateName(predicate2)
                    if "~" in predicateName:
                       negatedSentence2Predicate = predicateName.replace("~","")
                    else:
                        negatedSentence2Predicate = "~"+predicateName
                    if negatedSentence2Predicate == predicate2Name:
                        if canUnify(predicate,predicate2):
                            unificationPossible, unificationValues = unifyPredicates(predicate,predicate2)
                            if unificationPossible:
                                newPredicate1list=copyList(sentence1Predicates)
                                newPredicate1list.remove(predicate)
                                newPredicate2list=copyList(sentence2Predicates)
                                newPredicate2list.remove(predicate2)
                                newSentencePredicates = list(set(set(newPredicate1list).union(newPredicate2list)))
                                if not newSentencePredicates:
                                    isResolved = True
                                    return True
                                unifiedSentence = unifySentence("|".join(newSentencePredicates),unificationValues)
                                #unifiedSentence = standardiseVariable(unifiedSentence, m)
                                #m = m+1
                                    #elif (unifiedSentence, predicate,sentence2) not in loopstack:
                                     #   loopstack.append((unifiedSentence, predicate,sentence2))

                                if (predicate,  sentence2, predicate2) not in loopstack:
                                    #k = [i for i,x in enumerate(loopstack) if x == (unifiedSentence,unificationValues)]
                                    #index = len(loopstack)-k[0]-1
                                    #loopstack.pop()
                                    if not checkTautology(unifiedSentence):
                                        loopstack.append((predicate,  sentence2, predicate2))
                                        resolution1(KB, unifiedSentence)
                                        print loopstack[-1]
                                        #loopstack.pop()
                                #else:

                                if isResolved:
                                    return
                                    #print unifiedSentence


        i = i+1
    return

def standardiseVariable(sentence, line):
    result = []
    sentencePredicates = sentence.split("|")
    for predicate in sentencePredicates:
        predicateArguments = parseArgumentsfromPredicate(getArgumentList(predicate))
        newArgs=[]
        for (elementType, elementValue, elementLexpos) in predicateArguments:
            if elementType == 'VARIABLE':
                elementValue = elementValue+str(line)
            newArgs.append(elementValue)
        newArgsSentence = ", ".join(newArgs)
        result.append(getPredicateName(predicate)+"("+newArgsSentence+")")
    return "|".join(result)


def createnewKB():
    KB = {}
    for key in KBDict.keys():
        KB[key] = KBDict[key]
    return KB

def __main__(kb,queries):
    global isResolved
    global loopstack
    global m
    for i in xrange(len(kb)):
        logic = removeMultipleNot(parseSentence(kb[i], 0), kb[i])
        logic = removeImplicationinLogic(parseSentence(logic, i + 1))


            # removeImplicationinLogic(parseSentence(kb[i],i+1))
            # print newKB[i]
            # newKB[i] = moveNotInwards(newKB[i], parseSentence(newKB[i], i+1))
        if "&" in logic:
            print "postfix"
            cnfList = cnfConversion(infixToPostfix(logic))
            for sentence in cnfList:
                print sentence
                sentence = standardiseVariable(sentence, m)
                newKB.append(sentence)
                KBDict[m] = sentence
                m = m + 1
        else:
            logic = standardiseVariable(logic, m)
            newKB.append(logic)
            KBDict[m] = logic
            m = m + 1
    # print cnfKB
    clearallDict()
    print newKB
    print "KB dict"
    print KBDict
    makeCopiesofDict()
    allPredicateNameDict()

    for i in KBDict.keys():
        sentence = parseSentence(KBDict[i], 0)
        if len(sentence) <= 2 and sentence:
            truePredicateDict[i] = KBDict[i]

    #print truePredicateDict



    KB = createnewKB()
    for resolveQuery in queries:
        print resolveQuery
        loopstack=[]
        KB1 = {}
        isResolved = False
        #resolveQuery = "Ancestor(Liz,Billy)"
        if "~" in resolveQuery:
            resolveQuery = resolveQuery.replace("~", "")
        else:
            resolveQuery = "~" + resolveQuery
        #KB = createnewKB()

        KB[len(KB.keys()) + 1] = resolveQuery

        listsinKBbyLength = sorted(KB.values(), key=len)

        i = 1

        while i <= len(KB.keys()):
            KB1[i] = listsinKBbyLength[i - 1]
            i = i + 1

        # print resolution1(resolveQuery)
        loopstack.append(resolveQuery)
        resolution1(KB1, resolveQuery)
        print "resolved: ", isResolved

    # print unifyPredicates('P(x,y,John)','P(Sam,z,John)')


#__main__()


def readData():
    kb = []
    with open('input.txt') as f:
        kb = f.read().splitlines()
    numberOfQueries=int(kb[0])
    #print numberOfQueries
    kb=kb[1:]
    queries=[]
    for i in xrange(numberOfQueries):
        queries.append(kb[i])
    kb=kb[numberOfQueries+1:]
    print kb
    print queries
    __main__(kb,queries)


readData()
#print checkTautology("A(x,John)|B(x,y)|C(James)|A(x,John)")
#print unifyPredicates("A(x1,z1,y1)","A(x,x,y)")