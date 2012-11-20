# This file is part of PrettySargam.
#
# PrettySargam is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Foobar is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with PrettySargam.  If not, see <http://www.gnu.org/licenses/>.

import os
import re
import string
import sys 
from tkinter import filedialog
from tkinter import messagebox
import tkinter
import time
import subprocess, os

print(sys.argv)

class Note:
    def __init__(self, symbol):
        self.symbol = symbol

class Raga:
    def __init__(self, allowedNotes):
        self.allowedNotes = allowedNotes
    def isAllowed(self, symbol):
        s = symbol[0] # strip away octave specifier
        for note in self.allowedNotes:
            if note.symbol == s:
                return True
        return False

S = Note("S")
r = Note("r")
R = Note("R")
g = Note("g")
G = Note("G")
M = Note("M")
m = Note("m")
P = Note("P")
d = Note("d")
D = Note("D")
n = Note("n")
N = Note("N")

ragas = {
    "Bageshri": Raga([S, R, g, M, P, D, n]),
    "Bhairav": Raga([S, r, G, M, P, d, N]),
    "Bhairavi": Raga([S, r, R, g, M, P, d, n]),
    "Bhimpalasi": Raga([S, R, g, M, P, D, n]),
    "Bhupali": Raga([S, R, G, P, D]),
    "Bihag": Raga([S, R, G, M, m, P, D, N]),
    "Brindabani Sarang": Raga([S, R, M, P, N, n]),
    "Darbari": Raga([S, R, g, M, P, d, n]),
    "Desh": Raga([S, R, G, M, P, D, N, n]),
    "Durga": Raga([S, R, M, P, D]),
    "Jaunpuri": Raga([S, R, g, M, P, d, n]),
    "Jog": Raga([S, G, g, M, P, n]),
    "Kafi": Raga([S, R, g, M, P, D, n]),
    "Patdeep": Raga([S, R, g, M, P, D, N]),
    "Pilu": Raga([S, R, r, G, g, M, m, P, D, d, N, n]),
    "Todi": Raga([S, r, g, m, P, d, N]),
    "Yaman": Raga([S, R, G, m, P, D, N])
}

talas = {
    "Ektal": { "Vilambit": [ "Dhin", "Dhin", "|", "Dhage", "Tirakita", "|",
                             "Tun", "Na", "|", "Ka", "Ta", "|",
                             "Dhage", "Tirakita", "|", "Dhin", "Na"] },

    "Jhaptal": { "Madhya": [ "Dhin", "Na", "|", "Dhin", "Dhin", "Na", "|",
                             "Tin", "Na", "|", "Dhin", "Dhin", "Na" ] },

    "Jhumratal": { "Vilambit": [ "Dhin", "- Dha", "Tirkit", "|",
                                 "Dhin", "Dhin", "Dhage", "Tirkit", "|",
                                 "Tin", "- Tin", "Tirkit", "|",
                                 "Dhin", "Dhin", "Dhage", "Tirkit"] },

    "Mattatal": { "Madhya": ["Dhin", "Tirkit", "|", "Dhin", "Na", "|", "Tun", "Na", "|", "Dhin", "Dhin", "Na" ] },

    "Teental": { "Madhya": ["Dha", "Dhin", "Dhin", "Dha", "|",
                            "Dha", "Dhin", "Dhin", "Dha", "|",
                            "Dha", "Tin", "Tin", "Ta", "|",
                            "Ta", "Dhin", "Dhin", "Dha"] }

    }

DEV_MODE = False # sys.platform == "win32"

def showErrorAndQuit(msg):
    messagebox.showerror("Error", msg)
    sys.exit(1)

class Cycle:
    def __init__(self):
        self.measures = []
        self.caption = None

class Measure:
    def __init__(self):
        self.beats = []

SARGAM_PATTERN = r"[SRrGgMmPDdNn]"
NOTE_PATTERN = r"({0}[\.:'\"]?)".format(SARGAM_PATTERN)
SYMBOL_PATTERN = r"(\[{0}+\]{0}|{0}|-|\(|\)|,|;|\+)".format(NOTE_PATTERN)
POLYRYTHM_PATTERN = r"(^\d+\(.*?\))"
symbolRegex = re.compile(SYMBOL_PATTERN)
polyrythmRegex = re.compile(POLYRYTHM_PATTERN)

class Beat:
    def __init__(self, text, isCopied, isReplicated):
        self.text = text
        self.isCopied = isCopied
        self.isReplicated = isReplicated
        self.subBeats = 1
        if text != None:
            if text.count("(") != text.count(")"):
                showErrorAndQuit("Beat '{0}' is missing parenthesis.".format(text))
                
    def getSymbols(self):
        if self.text == None:
            return None
        unmatchedSymbols = symbolRegex.sub("", self.text)
        if len(unmatchedSymbols) > 0:
            showErrorAndQuit("Beat '{0}' is containing unmatched character(s): '{1}'.".format(self.text, unmatchedSymbols))
        symbols = []
        for g in symbolRegex.findall(self.text):
            symbols.append(g[0])
        return symbols

    def isPolyrythm(self):
        return self.subBeats > 1

    def isMeendedTo(self):
        symbols = self.getSymbols()
        return len(symbols) > 0 and symbols[0] == "+"

def beatIsBackreference(beat):
    return beat.text != None and len(beat.text) > 0 and beat.text[0] == ">"

def safeRemove(fileName):
    if os.path.exists(fileName):
        os.remove(fileName)

def parseBeats(text):
    beats = []
    text = text.lstrip()
    while len(text) > 0:
        text = text.lstrip()
        m = polyrythmRegex.match(text)
        isPolyrythm = m != None
        if not m:
            m = re.match(r"(^\S*)", text) # match up to next whitespace
        assert m
        beatText = m.groups()[0].rstrip()
        if isPolyrythm:
            subBeats = int(float(re.match("(^\d+)", beatText).groups()[0]))
        else:
            subBeats = 1
        for i in range(0, subBeats):
            beat = Beat(beatText, False, False)
            beat.subBeats = subBeats
            assert beat.isPolyrythm() == isPolyrythm
            beats.append(beat)
            
        text = text.replace(beatText, "", 1)
        text = text.lstrip()
    return beats

def symbolsAreEnclosedInParentheses(symbols):
    assert len(symbols) > 0
    if symbols[0] != "(":
        return False
    if symbols[-1] != ")":
        return False
    nbParenthesisToClose = 1
    for symbol in symbols[1:-2]:
        if symbol == "(":
            nbParenthesisToClose = nbParenthesisToClose + 1
        elif symbol == ")":
            nbParenthesisToClose = nbParenthesisToClose - 1
        if nbParenthesisToClose == 0: # the first parenthesis was closed
            return False
    return True

def beatToTex(beat, raga, inCompositionMode):
    text = ""
    if beat.text != None and beat.isPolyrythm():
        print("Polyrythm: " + beat.text)
        delimiter = " "
        if inCompositionMode:
            delimiter = " \hfill "
        tex = ""
        m = re.match(r"(\d+)\((.*)\)", beat.text)
        multiplier = m.groups()[0]
        subBeats = m.groups()[1]
        for subBeat in parseBeats(subBeats):
            print("Subbeat: " + subBeat.text)
            tex = tex + delimiter + beatToTex(subBeat, raga, inCompositionMode).strip("$")
        if inCompositionMode:
            tex = tex + delimiter
        text = tex.lstrip()
        #return r"$ \underfence{" + tex.lstrip() + r"}_{%s} $" % multiplier
    else:
        symbols = beat.getSymbols()
        if symbols == None:
            return None

        assert len(symbols) > 0
        noteCount = 0
        for symbol in symbols:
            print("symbol: " + symbol)
            notes = re.findall(NOTE_PATTERN, symbol) # decompose symbol in its notes (= grace notes + main note)
            symbolIsNote = len(notes) > 0
            if symbolIsNote or symbol == "-" or symbol == ";":
                noteCount = noteCount + 1
            t = re.sub(r"({0})".format(SARGAM_PATTERN), r"@\1", symbol) # prefix notes with special char so they can be found in later replaces
            t = re.sub(r"\[(.+?)\]", r"^{\1}", t)
            t = re.sub(r"(@{0}):".format(SARGAM_PATTERN), r"\\text{\\textsubumlaut{\1}}", t)
            t = re.sub(r"(@{0})\.".format(SARGAM_PATTERN), r"\underaccent{\dot}{\1}", t)
            t = re.sub(r"(@{0})'".format(SARGAM_PATTERN), r"\dot{\1}", t)
            t = re.sub(r"(@{0})\"".format(SARGAM_PATTERN), r"\ddot{\1}", t)
            t = re.sub(r"(@r\.?)", r"\underline{R}", t)
            t = re.sub(r"(@g\.?)", r"\underline{G}", t)
            t = re.sub(r"(@m\.?)", r"\\text{\\textvbaraccent{M}}", t)
            t = re.sub(r"(@d\.?)", r"\underline{D}", t)
            t = re.sub(r"(@n\.?)", r"\underline{N}", t)
            if symbolIsNote:
                t = r"\mathrm{" + t + "}"
            if not beat.isCopied and symbolIsNote:
                for note in notes:
                    if not raga.isAllowed(note):
                        t = "\color{red}" + t + "\color{black}"
                        break
            t = re.sub(r"@", r"", t) # remove special char again
            text = text + t
        if noteCount > 1 and not symbolsAreEnclosedInParentheses(symbols):
            text = "(" + text + ")"
            text = re.sub(r",\)$", r"),", text) # move trailing commas outside of underfence
        text = re.sub(r",", r"\\textbf{,}", text) # bold commas
        text = re.sub(r";", r"\\textbf{;}", text) # bold semi-colons
        while True: # underfences
            text2 = re.sub(r"\((.*?)\)", r"\underfence{\1}", text)
            if text2 == text: # nothing to replace anymore
                break
            text = text2
        while True: # overfences
            text2 = re.sub(r"([^+]+)\+([^+]+)", r"\overfence{\1 \2}", text)
            if text2 == text: # nothing to replace anymore
                break
            text = text2
        assert text == text2
    if beat.isCopied:
        text = "\color{gray} " + text + " \color{black}"
    return "$ " + text + " $"

def parsePhrases(text):
    phrases = []
    for phrase in text.split(","):
        phrases.append(phrase + ",")
    phrases[-1] = phrases[-1].rstrip(",") # remove trailing comma from last phrase
    return phrases

def snippetToTex(snippet, raga):
    tex = ""
    phrases = parsePhrases(snippet)
    phraseDelimiter = "" # the first phrase does not need to be prefixed with a space
    for phrase in phrases:
        beats = parseBeats(phrase)
        phraseTex = ""
        b = 0
        for b in range(0, len(beats)):
            beat = beats[b]
            beatTex = beatToTex(beat, raga, False)
            assert beatTex[0] == "$"
            assert beatTex[-1] == "$"
            nextBeatIsMeendedTo = b < len(beats) - 1 and beats[b + 1].isMeendedTo()
            if nextBeatIsMeendedTo:
                beatTex = r"$ \overfence{" + beatTex.strip("$") + " \mbox{  } "
            if b > 0 and beat.isMeendedTo(): # this beat is meended to
                if nextBeatIsMeendedTo:
                    showErrorAndQuit("Cannot meend to and starting from the same beat.")
                beatTex = beatTex.strip("$ +") + r"} $" # close overfence
            phraseTex = phraseTex + " " + beatTex
            b = b + 1
        tex = tex + r" \mbox{" + phraseDelimiter + phraseTex.strip() + "}" # prevent line-break in phrases using mbox
        phraseDelimiter = ""
    return tex.lstrip()

def lineIsPagebreak(line):
    return line.strip() == r"\pagebreak"

def lineStartsComposition(line):
    return line.startswith("Tala:")

def writeTitle(texFile, ragaName):
    texFile.write(r"\begin{center} {\LARGE Raga %s} \end{center}" % ragaName + os.linesep)

snippetRegex = re.compile(r"`(.*?)`")
def getSnippets(line):
    return snippetRegex.findall(line)
    
def processSnippets(lines, currentLine, raga, ragaName, texFile):

    writeTitle(texFile, ragaName)

    for line in lines[currentLine:len(lines)]:
        if lineIsPagebreak(line):
            break
        snippets = getSnippets(line)
        if len(snippets) > 0:
            for snippet in snippets:
                line = line.replace(snippet, snippetToTex(snippet, raga), 1)
            line = line.replace("`", "")
        texFile.write(line) # note: os.linesep is already contained in line
        currentLine = currentLine + 1

    return currentLine

def processComposition(lines, currentLine, raga, ragaName, texFile, writeRagaNameInTitle):

    # initialize tala
    talaName = lines[currentLine][len("Tala:"):-1].strip()
    print("Tala: '{0}'".format(talaName))
    currentLine = currentLine + 1

    if len(lines) < currentLine + 1 or not lines[currentLine].startswith("Laya:"):
        showErrorAndQuit("Laya is not defined.")
    layaText = lines[currentLine][len("Laya:"):-1].strip()
    layaName = re.match(r"(\w+)", layaText).groups()[0]
    print("Laya: '{0}'".format(layaName))
    currentLine = currentLine + 1

    if not talaName in talas:
        showErrorAndQuit("Tala '{0}' is not defined.".format(talaName))
    if not layaName in talas[talaName]:
        showErrorAndQuit("Laya '{0}' is not defined for tala '{1}'.".format(layaName, talaName))
    tala = talas[talaName][layaName]
    
    measureLengths = []
    m = 0
    for t in tala:
        if t == "|":
            measureLengths.append(m)
            m = 0
        else:
            m = m + 1
    measureLengths.append(m)
    beatsPerLine = sum(measureLengths)

    useLandscape = layaName == "Vilambit"
    if useLandscape:
        #texFile.write(r"\clearpage" + os.linesep)
        texFile.write(r"\newgeometry{bottom=0cm,top=0cm}" + os.linesep)
        texFile.write(r"\thispagestyle{empty}" + os.linesep)
        texFile.write(r"\begin{landscape}" + os.linesep)

    # write titles
    if writeRagaNameInTitle:
        writeTitle(texFile, ragaName)
    else:
        texFile.write(r"\mbox{}" + os.linesep) # add empty line before starting new composition
    texFile.write(r"\begin{{center}} Composition {0} -- {1} \end{{center}}".format(talaName, layaText) + os.linesep)

    # write table header(s)
    tabularHeader = ""
    talaNamesLine = ""
    beatIndexLine = ""
    b = 1
    for t in tala:
        if t == "|":
            tabularHeader = tabularHeader + " |"
        else:
            tabularHeader = tabularHeader + " c"
            talaNamesLine = talaNamesLine + r" {\scriptsize " + t + "}"
            beatIndexLine = beatIndexLine + r" {\footnotesize %d" % b + "}"
            if b < beatsPerLine:
                talaNamesLine = talaNamesLine + " &"
                beatIndexLine = beatIndexLine + " &"
            b = b + 1

    texFile.write(r"\begingroup" + os.linesep)
    if not useLandscape:
        # the following directive is used to center too wide tables (see: http://tex.stackexchange.com/questions/32726/center-wide-longtable-not-tabular-or-tabularx)
        texFile.write(r"\setlength{\LTleft}{-20cm plus -1fill}" + os.linesep)
        texFile.write(r"\setlength{\LTright}{\LTleft}" + os.linesep)       
    texFile.write(r"\begin{longtable}{%s }" % tabularHeader + os.linesep)
    texFile.write(talaNamesLine + r" \\" + os.linesep)
    texFile.write(beatIndexLine + r" \\" + os.linesep)
    texFile.write(r"\hline \\ [-10pt]" + os.linesep)
    texFile.write(r"\endhead" + os.linesep)
    
    def lineContainsNotes(line):
        return "/" in line

    # skip preceeding empty lines
    for line in lines[currentLine:len(lines)]:
        if line.strip() != '': # not empty line
            break
        currentLine = currentLine + 1

    # parse file into Cycles
    lastLineWasIncomplete = False
    cycles = []
    for line in lines[currentLine:len(lines)]:
        print("line: " + line)
        
        if lineIsPagebreak(line) or lineStartsComposition(line):
            break
        
        if lineContainsNotes(line) and lastLineWasIncomplete: # FIXME: a line might be part of composition but have no / only one measure
            showErrorAndQuit("Line {0} is missing measures. Use / to add a new measure. ".format(currentLine))
        
        cycle = Cycle()
        cycles.append(cycle)
        if line.strip() == '': # empty line
            lastLineWasIncomplete = False
        elif lineContainsNotes(line):
            m = 0
            for measureStr in line.split("/"):
                print("measure: '" + measureStr + "'")
                measure = Measure()
                measure.beats = parseBeats(measureStr)
                contentIsRightAligned = len(measureStr.lstrip()) < len(measureStr.rstrip())
                contentStartsAtBeginning = len(measureStr) - len(measureStr.lstrip()) < 2
                measure.leftFill = not contentStartsAtBeginning and contentIsRightAligned
                cycle.measures.append(measure)
                m = m + 1
            if m > len(measureLengths):
                showErrorAndQuit("Line {0} has too much measures.".format(currentLine))
            lastLineWasIncomplete = m < len(measureLengths) and not len(line.strip()) == 0
        else:
            cycle.caption = line.strip()

        currentLine = currentLine + 1

    # print cycles
    for cycle in cycles:
        print("cycle:")
        for measure in cycle.measures:
            print("\tmeasure:")
            for beat in measure.beats:
               print("\t\tbeat: " + beat.text)

    # transform cycles into beat sequence
    captions = {} # beat index -> text
    sthaiStart = -1
    antaraStart = -1
    lookForAntara = False
    beats = []
    for cycle in cycles:
        m = 0 # measure index
        for measure in cycle.measures:
            measureLength = measureLengths[m]
            if len(measure.beats) < measureLength:
                # fill missing beats
                for i in range(measureLength - len(measure.beats)):
                    if measure.leftFill:
                        measure.beats.insert(0, Beat(None, False, False))
                    else: # right fill
                        measure.beats.append(Beat(None, False, False))  
            elif len(measure.beats) > measureLength:
                showErrorAndQuit("Measure '{0}' {1} contains too much beats".format(measure.beats, m)) # FIXME: line number?
            
            assert len(measure.beats) == measureLengths[m]
            for beat in measure.beats:
                assert beat != None
                beats.append(beat)
                if beat.text != None:
                    if not lookForAntara and sthaiStart < 0:
                        sthaiStart = len(beats) - 1
                        captions[(sthaiStart // beatsPerLine) * beatsPerLine] = "Sthai"
                        print("Sthai starts at: {0}".format(sthaiStart))
                    elif lookForAntara and antaraStart < 0:
                        antaraStart = len(beats) - 1
                        captions[(antaraStart // beatsPerLine) * beatsPerLine] = "Antara"
                        print("Antara starts at: {0}".format(antaraStart))
            m = m + 1
        if len(cycle.measures) == 0: # was an empty line or caption
            lookForAntara = True
        else:
            # fill missing beats to complete the line
            for m in range(m, len(measureLengths)):
                for b in range(measureLengths[m]):
                    beats.append(Beat(None, False, False))
        assert len(beats) % beatsPerLine == 0
        if cycle.caption:            
            captions[len(beats)] = cycle.caption

    print("beats:")
    b = 0
    for beat in beats:
        print("{0}: {1}".format(b, beat.text))
        b = b + 1
        if b % beatsPerLine == 0:
            print("---------------------")

    # replicate back-references in beat sequence
    for b in range(1, len(beats)):
        if beatIsBackreference(beats[b - 1]) and beats[b].text == None:
            beats[b] = Beat(beats[b - 1].text, True, True)
            beats[b].subBeats = beats[b - 1].subBeats

    def replaceBackReference(beatIndex, startBeat):
        # When more replacements were made than the length of one cycle,
        # we need to find the correct offset into the next relevant
        # cycle. The next relevant cycle is one that was not copied
        # itself. For example: if someone references the Sthai for more
        # than one cycle, we have to make sure to continue with the outro
        # of the Sthai and not to re-copy the Sthai (if it was copied).
        cycleOffset = ((replacements // beatsPerLine) * beatsPerLine)
        while (True):
            i = int(((beatIndex - startBeat) % beatsPerLine) + startBeat + cycleOffset)
            if i >= len(beats): # there is no further relevant cycle
                # in this case, we will start from scratch again
                i = int(((beatIndex - startBeat) % beatsPerLine) + startBeat)
            referencedBeat = beats[i]
            if not referencedBeat.isCopied:
                break
            # the referencedBeat is a copy itself, so skip it
            cycleOffset = cycleOffset + beatsPerLine
        assert referencedBeat != None
        assert referencedBeat.text == None or not referencedBeat.text.startswith(">")
    
        #print "Replacing {0} with {1}".format(beats[b].text, sthaiBeat.text)                                        
        beats[beatIndex] = Beat(referencedBeat.text, True, referencedBeat.isReplicated)
        beats[beatIndex].subBeats = referencedBeat.subBeats
    
    # replace back-references with content
    replacements = 0
    lastReplacement = ""
    for b in range(len(beats)):
        beat = beats[b]
        if beatIsBackreference(beat):
            if not beat.isReplicated:
                replacements = 0 # restart counting when a new back-reference is made
            if beat.text == ">sth":
                if lastReplacement != ">sth":
                    lastReplacement = ">sth"
                    replacements = 0
                assert sthaiStart >= 0
                replaceBackReference(b, sthaiStart)
            elif beat.text == ">ant":
                if antaraStart < 0:
                    showErrorAndQuit("Antara has not been defined.")
                if lastReplacement != ">ant":
                    lastReplacement = ">ant"
                    replacements = 0
                replaceBackReference(b, antaraStart)
            else:
                showErrorAndQuit("Unknown reference '{0}'. Use 'sth' for sthai or 'ant' for antara.".format(beat.text))
            replacements = replacements + 1
        else:
            replacements = 0

    
    # write texFile
    print("Captions:")
    print(captions)

    b = 0
    while b < len(beats):
        beat = beats[b]
        if b in captions:
            # write caption
            print("Writing caption: " + captions[b])
            texFile.write(r"\multicolumn{%d" % beatsPerLine + r"}{l}{\small " + captions[b] + r"} \\*" + os.linesep)

        tex = beatToTex(beat, raga, True)
        if beat.isPolyrythm():
            assert beat.subBeats > 1
            tex = r"\multicolumn{%d" % beat.subBeats + r"}{|c|}{" + tex + r"}"
        nextBeatIndex = b + beat.subBeats
        
        if tex:
            texFile.write(tex)
        else:
            pass # empty line fillers

        def nextLineContainsOnlyBackreferences():
            if nextBeatIndex >= len(beats): # there is no next line
                return False
            for nextBeat in range(nextBeatIndex, nextBeatIndex + beatsPerLine):
                if nextBeat >= len(beats):
                    break # we are through with the line
                if not beats[nextBeat].isCopied:
                    return False
            return True
            
        def writingTanas():
            if len(captions) <= 2:
                return False # there are no Tanas
            currentLine = b // beatsPerLine
            # if we have past the 3rd caption, we are writing Tanas
            return currentLine > sorted(captions.keys())[2] // beatsPerLine

        if nextBeatIndex % beatsPerLine == 0:
            currentLineEndsWithBackreference = beat.isCopied
            nextLineStartsWithBackreference = nextBeatIndex < len(beats) and beats[nextBeatIndex].isCopied
            lineTerminatesCurrentTana = currentLineEndsWithBackreference or nextLineStartsWithBackreference
            extraRowSpace = "[4pt]"
            preventPageBreaking = "*"
            if nextBeatIndex in captions: # next line is a caption
                extraRowSpace = "[10pt]"
                preventPageBreaking = ""
            elif lineTerminatesCurrentTana and writingTanas() and not nextLineContainsOnlyBackreferences():
                extraRowSpace = "[10pt]" # delimit two Tanas               
                preventPageBreaking = ""
            texFile.write(r"\\{0}{1}".format(preventPageBreaking, extraRowSpace) + os.linesep)
        else:
            texFile.write(" & ")

        b = nextBeatIndex

    texFile.write(r"\end{longtable}" + os.linesep)    
    texFile.write(r"\endgroup" + os.linesep)

    if useLandscape:
        texFile.write(r"\end{landscape}" + os.linesep)
        texFile.write(r"\restoregeometry" + os.linesep)
        
    return currentLine

def processFile(txtFilename):
    txtFile = open(txtFilename, 'r')
    lines = txtFile.readlines()
    txtFile.close()

    # open output file for writing
    filenameWithoutExt = os.path.splitext(txtFilename)[0]
    texFilename = filenameWithoutExt + ".tex"
    texFile = open(texFilename, 'w')
    texFile.write(r"""\documentclass{letter}
\usepackage{mathtools}% http://ctan.org/pkg/mathtools
\usepackage{etoolbox}% http://ctan.org/pkg/etoolbox

% \overfence definition
\let\overfence\overbrace % \overfence is similar to \overbrace
\let\downfencefill\downbracefill % match components of \overbrace
\patchcmd{\overfence}{\downbracefill}{\downfencefill}{}{}% patch \overfence...
\patchcmd{\downfencefill}{\braceru \bracelu}{}{}{}%... and \downfencefill

% \underfence definition
\let\underfence\underbrace % \underfence is similar to \underbrace
\let\upfencefill\upbracefill % match components of \underbrace
\patchcmd{\underfence}{\upbracefill}{\upfencefill}{}{}% patch \underfence...
\patchcmd{\upfencefill}{\bracerd \braceld}{}{}{}%... and \upfencefill

\usepackage{color}
\definecolor{gray}{gray}{0.5}

\usepackage{tipa}
\usepackage{accents}

%\usepackage[T3,T1]{fontenc}
%\DeclareSymbolFont{tipa}{T3}{cmr}{m}{n}
%\DeclareMathAccent{\vbar}{\mathalpha}{tipa}{156}

%\setlength{\oddsidemargin}{0.3in}
%\setlength{\evensidemargin}{0.3in}

%\usepackage{lscape}
\usepackage{pdflscape}

\usepackage{longtable}

\usepackage[a4paper,bottom=3.5cm,top=3.5cm]{geometry}

\begin{document}""" + os.linesep
    )

    # parse file
    raga = None
    isNewPage = True
    currentLine = 0
    while currentLine < len(lines):

        # skip empty lines
        if lines[currentLine].strip() == "":
            currentLine = currentLine + 1
            continue
        
        # parse raga
        if lines[currentLine].startswith("Raga:"):
            previousRaga = raga
            ragaName = lines[currentLine][len("Raga:"):-1].strip()
            print("Raga: '{0}'".format(ragaName))
            currentLine = currentLine + 1
            if currentLine >= len(lines):
                break # file is processed
            
            # initialize raga
            if not ragaName in ragas:
                showErrorAndQuit("Raga '{0}' is not defined.".format(ragaName))
            raga = ragas[ragaName]
        elif raga == None:
            showErrorAndQuit("Raga is not defined.")
        assert raga != None

        if not lineStartsComposition(lines[currentLine]):
            print("Entering Snippet mode")
            currentLine = processSnippets(lines, currentLine, raga, ragaName, texFile)
        else:
            print("Entering Composition mode")
            writeRagaNameInTitle = previousRaga != raga and isNewPage
            currentLine = processComposition(lines, currentLine, raga, ragaName, texFile, writeRagaNameInTitle)
            
        if currentLine >= len(lines):
            break # file is processed

        if lineIsPagebreak(lines[currentLine]):
            isNewPage = True
            texFile.write(r"\pagebreak" + os.linesep)
            currentLine = currentLine + 1
        else:
            isNewPage = False
            
    texFile.write(r"\end{document}")
    texFile.close()

    #os.system('latex --src-specials "%s"' % texFilename)
    os.system('pdflatex "{0}" --output-directory="{1}"'.format(texFilename, os.path.dirname(texFilename)))
    if not DEV_MODE:
        safeRemove('%s.log' % filenameWithoutExt)
        #safeRemove('%s.aux' % filenameWithoutExt) # DO NOT REMOVE aux FILE - needed in subsequent runs to adjust table column widths
        safeRemove('%s.tex' % filenameWithoutExt)
##    pdfFilename = '%s.pdf' % filenameWithoutExt
##
##    time.sleep(3)
##
##    if os.name == 'mac':
##        subprocess.call(('open', pdfFilename))
##    elif os.name == 'nt':
##        subprocess.call(('start', pdfFilename), shell=True)
##    elif os.name == 'posix':
##        subprocess.call(('xdg-open', pdfFilename))
##
    
##    if sys.platform == "win32":
##        os.system('start "' + pdfFilename + '"')
##    else:
##        os.system('open "' + pdfFilename + '"')

def runUnitTests():
    assert symbolsAreEnclosedInParentheses(["("]) == False
    assert symbolsAreEnclosedInParentheses(["(", ")"]) == True
    assert symbolsAreEnclosedInParentheses(["(", "S", ")"]) == True
    assert symbolsAreEnclosedInParentheses(["(", "S", ")", "(", "G", ")"]) == False
    assert symbolsAreEnclosedInParentheses(["(", "(", "S", ")", "(", "G", ")", ")"]) == True
#
# MAIN
#
runUnitTests()

# try to read the settings
SETTINGS_FILE_NAME = ".sargam2pdf"
lastOpenedFile = None
try:
   tmpFile = open(SETTINGS_FILE_NAME, "r")
   lines = tmpFile.readlines()
   tmpFile.close()
   if len(lines) > 0:
       lastOpenedFile = lines[0].strip()
except IOError as e:
   pass

# don't show Tk's root window (see: http://www.ferg.org/thinking_in_tkinter/index.html)
root = tkinter.Tk()
root.withdraw()

# the following hack is from http://stackoverflow.com/questions/3375227/how-to-give-tkinter-file-dialog-focus
# Make it almost invisible - no decorations, 0 size, top left corner.
root.overrideredirect(True)
root.geometry('0x0+0+0')

# Show window again and lift it to top so it can get focus,
# otherwise dialogs will end up behind the terminal.
root.deiconify()
root.lift()
root.focus_force()

initialDir = None
if lastOpenedFile != None:
    initialDir = os.path.dirname(lastOpenedFile)[0]
filenames = filedialog.askopenfilename(parent=root, initialfile=lastOpenedFile, # ignored on Mac (see: http://tkinter.unpythonic.net/wiki/tkFileDialog)
                                         initialdir=initialDir, # on Mac the General Controls control panel allows the end user to override the application default directory (see: http://tkinter.unpythonic.net/wiki/tkFileDialog)
                                         multiple=True, # on Mac only available when Navigation Services are installed (see: http://tkinter.unpythonic.net/wiki/tkFileDialog)
                                         filetypes=[('text files', '.txt')])
txtFilenames = root.tk.splitlist(filenames) # see http://bugs.python.org/issue5712

if len(txtFilenames) == 0:
    sys.exit(0)

# try storing settings
try:
   tmpFile = open(SETTINGS_FILE_NAME, "w")
   tmpFile.write(txtFilenames[0])
   tmpFile.close()
except IOError as e:
   print("Warning: could not store settings.")

for txtFilename in txtFilenames:
    print(txtFilename)
    processFile(txtFilename)
    

