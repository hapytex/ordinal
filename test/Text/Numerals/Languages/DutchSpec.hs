module Text.Numerals.Languages.DutchSpec where

import Data.Text(Text)

import Test.Hspec(Spec)
import Text.Numerals.Languages.Dutch(dutch)
import Text.Numerals.LanguageTest(testLanguage)

spec :: Spec
spec = testLanguage "Dutch" dutch cardinals ordinals

cardinals :: [(Integer, Text)]
cardinals = [
    (233, "tweehonderddrieëndertig")
  , (377, "driehonderdzevenenzeventig")
  , (610, "zeshonderdtien")
  , (987, "negenhonderdzevenentachtig")
  , (1597, "duizendvijfhonderdzevenennegentig")
  , (2584, "tweeduizendvijfhonderdvierentachtig")
  , (4181, "vierduizendhonderdeenentachtig")
  , (6765, "zesduizendzevenhonderdvijfenzestig")
  , (10946, "tienduizendnegenhonderdzesenveertig")
  , (17711, "zeventienduizendzevenhonderdelf")
  , (28657, "achtentwintigduizendzeshonderdzevenenvijftig")
  , (46368, "zesenveertigduizenddriehonderdachtenzestig")
  , (75025, "vijfenzeventigduizendvijfentwintig")
  , (121393, "honderdeenentwintigduizenddriehonderddrieënnegentig")
  , (196418, "honderdzesennegentigduizendvierhonderdachttien")
  , (317811, "driehonderdzeventienduizendachthonderdelf")
  , (514229, "vijfhonderdveertienduizendtweehonderdnegenentwintig")
  , (832040, "achthonderdtweeëndertigduizendveertig")
  , (1346269, "een miljoen driehonderdzesenveertigduizendtweehonderdnegenenzestig")
  , (2178309, "twee miljoen honderdachtenzeventigduizenddriehonderdnegen")
  , (3524578, "drie miljoen vijfhonderdvierentwintigduizendvijfhonderdachtenzeventig")
  , (5702887, "vijf miljoen zevenhonderdtweeduizendachthonderdzevenentachtig")
  , (9227465, "negen miljoen tweehonderdzevenentwintigduizendvierhonderdvijfenzestig")
  , (14930352, "veertien miljoen negenhonderddertigduizenddriehonderdtweeënvijftig")
  , (24157817, "vierentwintig miljoen honderdzevenenvijftigduizendachthonderdzeventien")
  , (39088169, "negenendertig miljoen achtentachtigduizendhonderdnegenenzestig")
  , (63245986, "drieënzestig miljoen tweehonderdvijfenveertigduizendnegenhonderdzesentachtig")
  , (102334155, "honderdtwee miljoen driehonderdvierendertigduizendhonderdvijfenvijftig")
  , (165580141, "honderdvijfenzestig miljoen vijfhonderdtachtigduizendhonderdeenenveertig")
  , (267914296, "tweehonderdzevenenzestig miljoen negenhonderdveertienduizendtweehonderdzesennegentig")
  , (433494437, "vierhonderddrieëndertig miljoen vierhonderdvierennegentigduizendvierhonderdzevenendertig")
  , (701408733, "zevenhonderdéén miljoen vierhonderdachtduizendzevenhonderddrieëndertig")
  , (1134903170, "een miljard honderdvierendertig miljoen negenhonderddrieduizendhonderdzeventig")
  , (1836311903, "een miljard achthonderdzesendertig miljoen driehonderdelfduizendnegenhonderddrie")
  , (2971215073, "twee miljard negenhonderdeenenzeventig miljoen tweehonderdvijftienduizenddrieënzeventig")
  , (4807526976, "vier miljard achthonderdzeven miljoen vijfhonderdzesentwintigduizendnegenhonderdzesenzeventig")
  , (7778742049, "zeven miljard zevenhonderdachtenzeventig miljoen zevenhonderdtweeënveertigduizendnegenenveertig")
  , (12586269025, "twaalf miljard vijfhonderdzesentachtig miljoen tweehonderdnegenenzestigduizendvijfentwintig")
  , (20365011074, "twintig miljard driehonderdvijfenzestig miljoen elfduizendvierenzeventig")
  , (32951280099, "tweeëndertig miljard negenhonderdeenenvijftig miljoen tweehonderdtachtigduizendnegenennegentig")
  , (53316291173, "drieënvijftig miljard driehonderdzestien miljoen tweehonderdeenennegentigduizendhonderddrieënzeventig")
  , (86267571272, "zesentachtig miljard tweehonderdzevenenzestig miljoen vijfhonderdeenenzeventigduizendtweehonderdtweeënzeventig")
  , (139583862445, "honderdnegenendertig miljard vijfhonderddrieëntachtig miljoen achthonderdtweeënzestigduizendvierhonderdvijfenveertig")
  , (225851433717, "tweehonderdvijfentwintig miljard achthonderdeenenvijftig miljoen vierhonderddrieëndertigduizendzevenhonderdzeventien")
  , (365435296162, "driehonderdvijfenzestig miljard vierhonderdvijfendertig miljoen tweehonderdzesennegentigduizendhonderdtweeënzestig")
  , (591286729879, "vijfhonderdeenennegentig miljard tweehonderdzesentachtig miljoen zevenhonderdnegenentwintigduizendachthonderdnegenenzeventig")
  , (956722026041, "negenhonderdzesenvijftig miljard zevenhonderdtweeëntwintig miljoen zesentwintigduizendeenenveertig")
  , (1548008755920, "een biljoen vijfhonderdachtenveertig miljard acht miljoen zevenhonderdvijfenvijftigduizendnegenhonderdtwintig")
  , (2504730781961, "twee biljoen vijfhonderdvier miljard zevenhonderddertig miljoen zevenhonderdeenentachtigduizendnegenhonderdeenenzestig")
  , (4052739537881, "vier biljoen tweeënvijftig miljard zevenhonderdnegenendertig miljoen vijfhonderdzevenendertigduizendachthonderdeenentachtig")
  , (6557470319842, "zes biljoen vijfhonderdzevenenvijftig miljard vierhonderdzeventig miljoen driehonderdnegentienduizendachthonderdtweeënveertig")
  , (10610209857723, "tien biljoen zeshonderdtien miljard tweehonderdnegen miljoen achthonderdzevenenvijftigduizendzevenhonderddrieëntwintig")
  , (17167680177565, "zeventien biljoen honderdzevenenzestig miljard zeshonderdtachtig miljoen honderdzevenenzeventigduizendvijfhonderdvijfenzestig")
  , (27777890035288, "zevenentwintig biljoen zevenhonderdzevenenzeventig miljard achthonderdnegentig miljoen vijfendertigduizendtweehonderdachtentachtig")
  , (44945570212853, "vierenveertig biljoen negenhonderdvijfenveertig miljard vijfhonderdzeventig miljoen tweehonderdtwaalfduizendachthonderddrieënvijftig")
  , (72723460248141, "tweeënzeventig biljoen zevenhonderddrieëntwintig miljard vierhonderdzestig miljoen tweehonderdachtenveertigduizendhonderdeenenveertig")
  , (117669030460994, "honderdzeventien biljoen zeshonderdnegenenzestig miljard dertig miljoen vierhonderdzestigduizendnegenhonderdvierennegentig")
  , (190392490709135, "honderdnegentig biljoen driehonderdtweeënnegentig miljard vierhonderdnegentig miljoen zevenhonderdnegenduizendhonderdvijfendertig")
  , (308061521170129, "driehonderdacht biljoen eenenzestig miljard vijfhonderdeenentwintig miljoen honderdzeventigduizendhonderdnegenentwintig")
  , (498454011879264, "vierhonderdachtennegentig biljoen vierhonderdvierenvijftig miljard elf miljoen achthonderdnegenenzeventigduizendtweehonderdvierenzestig")
  , (806515533049393, "achthonderdzes biljoen vijfhonderdvijftien miljard vijfhonderddrieëndertig miljoen negenenveertigduizenddriehonderddrieënnegentig")
  , (1304969544928657, "een biljard driehonderdvier biljoen negenhonderdnegenenzestig miljard vijfhonderdvierenveertig miljoen negenhonderdachtentwintigduizendzeshonderdzevenenvijftig")
  , (2111485077978050, "twee biljard honderdelf biljoen vierhonderdvijfentachtig miljard zevenenzeventig miljoen negenhonderdachtenzeventigduizendvijftig")
  , (3416454622906707, "drie biljard vierhonderdzestien biljoen vierhonderdvierenvijftig miljard zeshonderdtweeëntwintig miljoen negenhonderdzesduizendzevenhonderdzeven")
  , (5527939700884757, "vijf biljard vijfhonderdzevenentwintig biljoen negenhonderdnegenendertig miljard zevenhonderd miljoen achthonderdvierentachtigduizendzevenhonderdzevenenvijftig")
  , (8944394323791464, "acht biljard negenhonderdvierenveertig biljoen driehonderdvierennegentig miljard driehonderddrieëntwintig miljoen zevenhonderdeenennegentigduizendvierhonderdvierenzestig")
  , (14472334024676221, "veertien biljard vierhonderdtweeënzeventig biljoen driehonderdvierendertig miljard vierentwintig miljoen zeshonderdzesenzeventigduizendtweehonderdeenentwintig")
  , (23416728348467685, "drieëntwintig biljard vierhonderdzestien biljoen zevenhonderdachtentwintig miljard driehonderdachtenveertig miljoen vierhonderdzevenenzestigduizendzeshonderdvijfentachtig")
  , (37889062373143906, "zevenendertig biljard achthonderdnegenentachtig biljoen tweeënzestig miljard driehonderddrieënzeventig miljoen honderddrieënveertigduizendnegenhonderdzes")
  , (61305790721611591, "eenenzestig biljard driehonderdvijf biljoen zevenhonderdnegentig miljard zevenhonderdeenentwintig miljoen zeshonderdelfduizendvijfhonderdeenennegentig")
  , (99194853094755497, "negenennegentig biljard honderdvierennegentig biljoen achthonderddrieënvijftig miljard vierennegentig miljoen zevenhonderdvijfenvijftigduizendvierhonderdzevenennegentig")
  , (160500643816367088, "honderdzestig biljard vijfhonderd biljoen zeshonderddrieënveertig miljard achthonderdzestien miljoen driehonderdzevenenzestigduizendachtentachtig")
  , (259695496911122585, "tweehonderdnegenenvijftig biljard zeshonderdvijfennegentig biljoen vierhonderdzesennegentig miljard negenhonderdelf miljoen honderdtweeëntwintigduizendvijfhonderdvijfentachtig")
  , (420196140727489673, "vierhonderdtwintig biljard honderdzesennegentig biljoen honderdveertig miljard zevenhonderdzevenentwintig miljoen vierhonderdnegenentachtigduizendzeshonderddrieënzeventig")
  , (679891637638612258, "zeshonderdnegenenzeventig biljard achthonderdeenennegentig biljoen zeshonderdzevenendertig miljard zeshonderdachtendertig miljoen zeshonderdtwaalfduizendtweehonderdachtenvijftig")
  , (1100087778366101931, "een triljoen honderd biljard zevenentachtig biljoen zevenhonderdachtenzeventig miljard driehonderdzesenzestig miljoen honderdéénduizendnegenhonderdeenendertig")
  , (1779979416004714189, "een triljoen zevenhonderdnegenenzeventig biljard negenhonderdnegenenzeventig biljoen vierhonderdzestien miljard vier miljoen zevenhonderdveertienduizendhonderdnegenentachtig")
  , (2880067194370816120, "twee triljoen achthonderdtachtig biljard zevenenzestig biljoen honderdvierennegentig miljard driehonderdzeventig miljoen achthonderdzestienduizendhonderdtwintig")
  , (4660046610375530309, "vier triljoen zeshonderdzestig biljard zesenveertig biljoen zeshonderdtien miljard driehonderdvijfenzeventig miljoen vijfhonderddertigduizenddriehonderdnegen")
  , (7540113804746346429, "zeven triljoen vijfhonderdveertig biljard honderddertien biljoen achthonderdvier miljard zevenhonderdzesenveertig miljoen driehonderdzesenveertigduizendvierhonderdnegenentwintig")
  , (12200160415121876738, "twaalf triljoen tweehonderd biljard honderdzestig biljoen vierhonderdvijftien miljard honderdeenentwintig miljoen achthonderdzesenzeventigduizendzevenhonderdachtendertig")
  , (19740274219868223167, "negentien triljoen zevenhonderdveertig biljard tweehonderdvierenzeventig biljoen tweehonderdnegentien miljard achthonderdachtenzestig miljoen tweehonderddrieëntwintigduizendhonderdzevenenzestig")
  , (31940434634990099905, "eenendertig triljoen negenhonderdveertig biljard vierhonderdvierendertig biljoen zeshonderdvierendertig miljard negenhonderdnegentig miljoen negenennegentigduizendnegenhonderdvijf")
  , (51680708854858323072, "eenenvijftig triljoen zeshonderdtachtig biljard zevenhonderdacht biljoen achthonderdvierenvijftig miljard achthonderdachtenvijftig miljoen driehonderddrieëntwintigduizendtweeënzeventig")
  , (83621143489848422977, "drieëntachtig triljoen zeshonderdeenentwintig biljard honderddrieënveertig biljoen vierhonderdnegenentachtig miljard achthonderdachtenveertig miljoen vierhonderdtweeëntwintigduizendnegenhonderdzevenenzeventig")
  , (135301852344706746049, "honderdvijfendertig triljoen driehonderdéén biljard achthonderdtweeënvijftig biljoen driehonderdvierenveertig miljard zevenhonderdzes miljoen zevenhonderdzesenveertigduizendnegenenveertig")
  , (218922995834555169026, "tweehonderdachttien triljoen negenhonderdtweeëntwintig biljard negenhonderdvijfennegentig biljoen achthonderdvierendertig miljard vijfhonderdvijfenvijftig miljoen honderdnegenenzestigduizendzesentwintig")
  , (354224848179261915075, "driehonderdvierenvijftig triljoen tweehonderdvierentwintig biljard achthonderdachtenveertig biljoen honderdnegenenzeventig miljard tweehonderdeenenzestig miljoen negenhonderdvijftienduizendvijfenzeventig")
  , (573147844013817084101, "vijfhonderddrieënzeventig triljoen honderdzevenenveertig biljard achthonderdvierenveertig biljoen dertien miljard achthonderdzeventien miljoen vierentachtigduizendhonderdéén")
  , (927372692193078999176, "negenhonderdzevenentwintig triljoen driehonderdtweeënzeventig biljard zeshonderdtweeënnegentig biljoen honderddrieënnegentig miljard achtenzeventig miljoen negenhonderdnegenennegentigduizendhonderdzesenzeventig")
  , (1500520536206896083277, "een triljard vijfhonderd triljoen vijfhonderdtwintig biljard vijfhonderdzesendertig biljoen tweehonderdzes miljard achthonderdzesennegentig miljoen drieëntachtigduizendtweehonderdzevenenzeventig")
  , (2427893228399975082453, "twee triljard vierhonderdzevenentwintig triljoen achthonderddrieënnegentig biljard tweehonderdachtentwintig biljoen driehonderdnegenennegentig miljard negenhonderdvijfenzeventig miljoen tweeëntachtigduizendvierhonderddrieënvijftig")
  , (3928413764606871165730, "drie triljard negenhonderdachtentwintig triljoen vierhonderddertien biljard zevenhonderdvierenzestig biljoen zeshonderdzes miljard achthonderdeenenzeventig miljoen honderdvijfenzestigduizendzevenhonderddertig")
  , (6356306993006846248183, "zes triljard driehonderdzesenvijftig triljoen driehonderdzes biljard negenhonderddrieënnegentig biljoen zes miljard achthonderdzesenveertig miljoen tweehonderdachtenveertigduizendhonderddrieëntachtig")
  , (10284720757613717413913, "tien triljard tweehonderdvierentachtig triljoen zevenhonderdtwintig biljard zevenhonderdzevenenvijftig biljoen zeshonderddertien miljard zevenhonderdzeventien miljoen vierhonderddertienduizendnegenhonderddertien")
  , (16641027750620563662096, "zestien triljard zeshonderdeenenveertig triljoen zevenentwintig biljard zevenhonderdvijftig biljoen zeshonderdtwintig miljard vijfhonderddrieënzestig miljoen zeshonderdtweeënzestigduizendzesennegentig")
  , (26925748508234281076009, "zesentwintig triljard negenhonderdvijfentwintig triljoen zevenhonderdachtenveertig biljard vijfhonderdacht biljoen tweehonderdvierendertig miljard tweehonderdeenentachtig miljoen zesenzeventigduizendnegen")
  , (43566776258854844738105, "drieënveertig triljard vijfhonderdzesenzestig triljoen zevenhonderdzesenzeventig biljard tweehonderdachtenvijftig biljoen achthonderdvierenvijftig miljard achthonderdvierenveertig miljoen zevenhonderdachtendertigduizendhonderdvijf")
  , (70492524767089125814114, "zeventig triljard vierhonderdtweeënnegentig triljoen vijfhonderdvierentwintig biljard zevenhonderdzevenenzestig biljoen negenentachtig miljard honderdvijfentwintig miljoen achthonderdveertienduizendhonderdveertien")
  , (114059301025943970552219, "honderdveertien triljard negenenvijftig triljoen driehonderdéén biljard vijfentwintig biljoen negenhonderddrieënveertig miljard negenhonderdzeventig miljoen vijfhonderdtweeënvijftigduizendtweehonderdnegentien")
  ]

ordinals :: [(Integer, Text)]
ordinals = [
    (233, "tweehonderddrieëndertigste")
  , (377, "driehonderdzevenenzeventigste")
  , (610, "zeshonderdtiende")
  , (987, "negenhonderdzevenentachtigste")
  , (1597, "duizendvijfhonderdzevenennegentigste")
  , (2584, "tweeduizendvijfhonderdvierentachtigste")
  , (4181, "vierduizendhonderdeenentachtigste")
  , (6765, "zesduizendzevenhonderdvijfenzestigste")
  , (10946, "tienduizendnegenhonderdzesenveertigste")
  , (17711, "zeventienduizendzevenhonderdelfde")
  , (28657, "achtentwintigduizendzeshonderdzevenenvijftigste")
  , (46368, "zesenveertigduizenddriehonderdachtenzestigste")
  , (75025, "vijfenzeventigduizendvijfentwintigste")
  , (121393, "honderdeenentwintigduizenddriehonderddrieënnegentigste")
  , (196418, "honderdzesennegentigduizendvierhonderdachttiende")
  , (317811, "driehonderdzeventienduizendachthonderdelfde")
  , (514229, "vijfhonderdveertienduizendtweehonderdnegenentwintigste")
  , (832040, "achthonderdtweeëndertigduizendveertigste")
  , (1346269, "een miljoen driehonderdzesenveertigduizendtweehonderdnegenenzestigste")
  , (2178309, "twee miljoen honderdachtenzeventigduizenddriehonderdnegende")
  , (3524578, "drie miljoen vijfhonderdvierentwintigduizendvijfhonderdachtenzeventigste")
  , (5702887, "vijf miljoen zevenhonderdtweeduizendachthonderdzevenentachtigste")
  , (9227465, "negen miljoen tweehonderdzevenentwintigduizendvierhonderdvijfenzestigste")
  , (14930352, "veertien miljoen negenhonderddertigduizenddriehonderdtweeënvijftigste")
  , (24157817, "vierentwintig miljoen honderdzevenenvijftigduizendachthonderdzeventiende")
  , (39088169, "negenendertig miljoen achtentachtigduizendhonderdnegenenzestigste")
  , (63245986, "drieënzestig miljoen tweehonderdvijfenveertigduizendnegenhonderdzesentachtigste")
  , (102334155, "honderdtwee miljoen driehonderdvierendertigduizendhonderdvijfenvijftigste")
  , (165580141, "honderdvijfenzestig miljoen vijfhonderdtachtigduizendhonderdeenenveertigste")
  , (267914296, "tweehonderdzevenenzestig miljoen negenhonderdveertienduizendtweehonderdzesennegentigste")
  , (433494437, "vierhonderddrieëndertig miljoen vierhonderdvierennegentigduizendvierhonderdzevenendertigste")
  , (701408733, "zevenhonderdéén miljoen vierhonderdachtduizendzevenhonderddrieëndertigste")
  , (1134903170, "een miljard honderdvierendertig miljoen negenhonderddrieduizendhonderdzeventigste")
  , (1836311903, "een miljard achthonderdzesendertig miljoen driehonderdelfduizendnegenhonderdderde")
  , (2971215073, "twee miljard negenhonderdeenenzeventig miljoen tweehonderdvijftienduizenddrieënzeventigste")
  , (4807526976, "vier miljard achthonderdzeven miljoen vijfhonderdzesentwintigduizendnegenhonderdzesenzeventigste")
  , (7778742049, "zeven miljard zevenhonderdachtenzeventig miljoen zevenhonderdtweeënveertigduizendnegenenveertigste")
  , (12586269025, "twaalf miljard vijfhonderdzesentachtig miljoen tweehonderdnegenenzestigduizendvijfentwintigste")
  , (20365011074, "twintig miljard driehonderdvijfenzestig miljoen elfduizendvierenzeventigste")
  , (32951280099, "tweeëndertig miljard negenhonderdeenenvijftig miljoen tweehonderdtachtigduizendnegenennegentigste")
  , (53316291173, "drieënvijftig miljard driehonderdzestien miljoen tweehonderdeenennegentigduizendhonderddrieënzeventigste")
  , (86267571272, "zesentachtig miljard tweehonderdzevenenzestig miljoen vijfhonderdeenenzeventigduizendtweehonderdtweeënzeventigste")
  , (139583862445, "honderdnegenendertig miljard vijfhonderddrieëntachtig miljoen achthonderdtweeënzestigduizendvierhonderdvijfenveertigste")
  , (225851433717, "tweehonderdvijfentwintig miljard achthonderdeenenvijftig miljoen vierhonderddrieëndertigduizendzevenhonderdzeventiende")
  , (365435296162, "driehonderdvijfenzestig miljard vierhonderdvijfendertig miljoen tweehonderdzesennegentigduizendhonderdtweeënzestigste")
  , (591286729879, "vijfhonderdeenennegentig miljard tweehonderdzesentachtig miljoen zevenhonderdnegenentwintigduizendachthonderdnegenenzeventigste")
  , (956722026041, "negenhonderdzesenvijftig miljard zevenhonderdtweeëntwintig miljoen zesentwintigduizendeenenveertigste")
  , (1548008755920, "een biljoen vijfhonderdachtenveertig miljard acht miljoen zevenhonderdvijfenvijftigduizendnegenhonderdtwintigste")
  , (2504730781961, "twee biljoen vijfhonderdvier miljard zevenhonderddertig miljoen zevenhonderdeenentachtigduizendnegenhonderdeenenzestigste")
  , (4052739537881, "vier biljoen tweeënvijftig miljard zevenhonderdnegenendertig miljoen vijfhonderdzevenendertigduizendachthonderdeenentachtigste")
  , (6557470319842, "zes biljoen vijfhonderdzevenenvijftig miljard vierhonderdzeventig miljoen driehonderdnegentienduizendachthonderdtweeënveertigste")
  , (10610209857723, "tien biljoen zeshonderdtien miljard tweehonderdnegen miljoen achthonderdzevenenvijftigduizendzevenhonderddrieëntwintigste")
  , (17167680177565, "zeventien biljoen honderdzevenenzestig miljard zeshonderdtachtig miljoen honderdzevenenzeventigduizendvijfhonderdvijfenzestigste")
  , (27777890035288, "zevenentwintig biljoen zevenhonderdzevenenzeventig miljard achthonderdnegentig miljoen vijfendertigduizendtweehonderdachtentachtigste")
  , (44945570212853, "vierenveertig biljoen negenhonderdvijfenveertig miljard vijfhonderdzeventig miljoen tweehonderdtwaalfduizendachthonderddrieënvijftigste")
  , (72723460248141, "tweeënzeventig biljoen zevenhonderddrieëntwintig miljard vierhonderdzestig miljoen tweehonderdachtenveertigduizendhonderdeenenveertigste")
  , (117669030460994, "honderdzeventien biljoen zeshonderdnegenenzestig miljard dertig miljoen vierhonderdzestigduizendnegenhonderdvierennegentigste")
  , (190392490709135, "honderdnegentig biljoen driehonderdtweeënnegentig miljard vierhonderdnegentig miljoen zevenhonderdnegenduizendhonderdvijfendertigste")
  , (308061521170129, "driehonderdacht biljoen eenenzestig miljard vijfhonderdeenentwintig miljoen honderdzeventigduizendhonderdnegenentwintigste")
  , (498454011879264, "vierhonderdachtennegentig biljoen vierhonderdvierenvijftig miljard elf miljoen achthonderdnegenenzeventigduizendtweehonderdvierenzestigste")
  , (806515533049393, "achthonderdzes biljoen vijfhonderdvijftien miljard vijfhonderddrieëndertig miljoen negenenveertigduizenddriehonderddrieënnegentigste")
  , (1304969544928657, "een biljard driehonderdvier biljoen negenhonderdnegenenzestig miljard vijfhonderdvierenveertig miljoen negenhonderdachtentwintigduizendzeshonderdzevenenvijftigste")
  , (2111485077978050, "twee biljard honderdelf biljoen vierhonderdvijfentachtig miljard zevenenzeventig miljoen negenhonderdachtenzeventigduizendvijftigste")
  , (3416454622906707, "drie biljard vierhonderdzestien biljoen vierhonderdvierenvijftig miljard zeshonderdtweeëntwintig miljoen negenhonderdzesduizendzevenhonderdzevende")
  , (5527939700884757, "vijf biljard vijfhonderdzevenentwintig biljoen negenhonderdnegenendertig miljard zevenhonderd miljoen achthonderdvierentachtigduizendzevenhonderdzevenenvijftigste")
  , (8944394323791464, "acht biljard negenhonderdvierenveertig biljoen driehonderdvierennegentig miljard driehonderddrieëntwintig miljoen zevenhonderdeenennegentigduizendvierhonderdvierenzestigste")
  , (14472334024676221, "veertien biljard vierhonderdtweeënzeventig biljoen driehonderdvierendertig miljard vierentwintig miljoen zeshonderdzesenzeventigduizendtweehonderdeenentwintigste")
  , (23416728348467685, "drieëntwintig biljard vierhonderdzestien biljoen zevenhonderdachtentwintig miljard driehonderdachtenveertig miljoen vierhonderdzevenenzestigduizendzeshonderdvijfentachtigste")
  , (37889062373143906, "zevenendertig biljard achthonderdnegenentachtig biljoen tweeënzestig miljard driehonderddrieënzeventig miljoen honderddrieënveertigduizendnegenhonderdzesde")
  , (61305790721611591, "eenenzestig biljard driehonderdvijf biljoen zevenhonderdnegentig miljard zevenhonderdeenentwintig miljoen zeshonderdelfduizendvijfhonderdeenennegentigste")
  , (99194853094755497, "negenennegentig biljard honderdvierennegentig biljoen achthonderddrieënvijftig miljard vierennegentig miljoen zevenhonderdvijfenvijftigduizendvierhonderdzevenennegentigste")
  , (160500643816367088, "honderdzestig biljard vijfhonderd biljoen zeshonderddrieënveertig miljard achthonderdzestien miljoen driehonderdzevenenzestigduizendachtentachtigste")
  , (259695496911122585, "tweehonderdnegenenvijftig biljard zeshonderdvijfennegentig biljoen vierhonderdzesennegentig miljard negenhonderdelf miljoen honderdtweeëntwintigduizendvijfhonderdvijfentachtigste")
  , (420196140727489673, "vierhonderdtwintig biljard honderdzesennegentig biljoen honderdveertig miljard zevenhonderdzevenentwintig miljoen vierhonderdnegenentachtigduizendzeshonderddrieënzeventigste")
  , (679891637638612258, "zeshonderdnegenenzeventig biljard achthonderdeenennegentig biljoen zeshonderdzevenendertig miljard zeshonderdachtendertig miljoen zeshonderdtwaalfduizendtweehonderdachtenvijftigste")
  , (1100087778366101931, "een triljoen honderd biljard zevenentachtig biljoen zevenhonderdachtenzeventig miljard driehonderdzesenzestig miljoen honderdéénduizendnegenhonderdeenendertigste")
  , (1779979416004714189, "een triljoen zevenhonderdnegenenzeventig biljard negenhonderdnegenenzeventig biljoen vierhonderdzestien miljard vier miljoen zevenhonderdveertienduizendhonderdnegenentachtigste")
  , (2880067194370816120, "twee triljoen achthonderdtachtig biljard zevenenzestig biljoen honderdvierennegentig miljard driehonderdzeventig miljoen achthonderdzestienduizendhonderdtwintigste")
  , (4660046610375530309, "vier triljoen zeshonderdzestig biljard zesenveertig biljoen zeshonderdtien miljard driehonderdvijfenzeventig miljoen vijfhonderddertigduizenddriehonderdnegende")
  , (7540113804746346429, "zeven triljoen vijfhonderdveertig biljard honderddertien biljoen achthonderdvier miljard zevenhonderdzesenveertig miljoen driehonderdzesenveertigduizendvierhonderdnegenentwintigste")
  , (12200160415121876738, "twaalf triljoen tweehonderd biljard honderdzestig biljoen vierhonderdvijftien miljard honderdeenentwintig miljoen achthonderdzesenzeventigduizendzevenhonderdachtendertigste")
  , (19740274219868223167, "negentien triljoen zevenhonderdveertig biljard tweehonderdvierenzeventig biljoen tweehonderdnegentien miljard achthonderdachtenzestig miljoen tweehonderddrieëntwintigduizendhonderdzevenenzestigste")
  , (31940434634990099905, "eenendertig triljoen negenhonderdveertig biljard vierhonderdvierendertig biljoen zeshonderdvierendertig miljard negenhonderdnegentig miljoen negenennegentigduizendnegenhonderdvijfde")
  , (51680708854858323072, "eenenvijftig triljoen zeshonderdtachtig biljard zevenhonderdacht biljoen achthonderdvierenvijftig miljard achthonderdachtenvijftig miljoen driehonderddrieëntwintigduizendtweeënzeventigste")
  , (83621143489848422977, "drieëntachtig triljoen zeshonderdeenentwintig biljard honderddrieënveertig biljoen vierhonderdnegenentachtig miljard achthonderdachtenveertig miljoen vierhonderdtweeëntwintigduizendnegenhonderdzevenenzeventigste")
  , (135301852344706746049, "honderdvijfendertig triljoen driehonderdéén biljard achthonderdtweeënvijftig biljoen driehonderdvierenveertig miljard zevenhonderdzes miljoen zevenhonderdzesenveertigduizendnegenenveertigste")
  , (218922995834555169026, "tweehonderdachttien triljoen negenhonderdtweeëntwintig biljard negenhonderdvijfennegentig biljoen achthonderdvierendertig miljard vijfhonderdvijfenvijftig miljoen honderdnegenenzestigduizendzesentwintigste")
  , (354224848179261915075, "driehonderdvierenvijftig triljoen tweehonderdvierentwintig biljard achthonderdachtenveertig biljoen honderdnegenenzeventig miljard tweehonderdeenenzestig miljoen negenhonderdvijftienduizendvijfenzeventigste")
  , (573147844013817084101, "vijfhonderddrieënzeventig triljoen honderdzevenenveertig biljard achthonderdvierenveertig biljoen dertien miljard achthonderdzeventien miljoen vierentachtigduizendhonderdeerste")
  , (927372692193078999176, "negenhonderdzevenentwintig triljoen driehonderdtweeënzeventig biljard zeshonderdtweeënnegentig biljoen honderddrieënnegentig miljard achtenzeventig miljoen negenhonderdnegenennegentigduizendhonderdzesenzeventigste")
  , (1500520536206896083277, "een triljard vijfhonderd triljoen vijfhonderdtwintig biljard vijfhonderdzesendertig biljoen tweehonderdzes miljard achthonderdzesennegentig miljoen drieëntachtigduizendtweehonderdzevenenzeventigste")
  , (2427893228399975082453, "twee triljard vierhonderdzevenentwintig triljoen achthonderddrieënnegentig biljard tweehonderdachtentwintig biljoen driehonderdnegenennegentig miljard negenhonderdvijfenzeventig miljoen tweeëntachtigduizendvierhonderddrieënvijftigste")
  , (3928413764606871165730, "drie triljard negenhonderdachtentwintig triljoen vierhonderddertien biljard zevenhonderdvierenzestig biljoen zeshonderdzes miljard achthonderdeenenzeventig miljoen honderdvijfenzestigduizendzevenhonderddertigste")
  , (6356306993006846248183, "zes triljard driehonderdzesenvijftig triljoen driehonderdzes biljard negenhonderddrieënnegentig biljoen zes miljard achthonderdzesenveertig miljoen tweehonderdachtenveertigduizendhonderddrieëntachtigste")
  , (10284720757613717413913, "tien triljard tweehonderdvierentachtig triljoen zevenhonderdtwintig biljard zevenhonderdzevenenvijftig biljoen zeshonderddertien miljard zevenhonderdzeventien miljoen vierhonderddertienduizendnegenhonderddertiende")
  , (16641027750620563662096, "zestien triljard zeshonderdeenenveertig triljoen zevenentwintig biljard zevenhonderdvijftig biljoen zeshonderdtwintig miljard vijfhonderddrieënzestig miljoen zeshonderdtweeënzestigduizendzesennegentigste")
  , (26925748508234281076009, "zesentwintig triljard negenhonderdvijfentwintig triljoen zevenhonderdachtenveertig biljard vijfhonderdacht biljoen tweehonderdvierendertig miljard tweehonderdeenentachtig miljoen zesenzeventigduizendnegende")
  , (43566776258854844738105, "drieënveertig triljard vijfhonderdzesenzestig triljoen zevenhonderdzesenzeventig biljard tweehonderdachtenvijftig biljoen achthonderdvierenvijftig miljard achthonderdvierenveertig miljoen zevenhonderdachtendertigduizendhonderdvijfde")
  , (70492524767089125814114, "zeventig triljard vierhonderdtweeënnegentig triljoen vijfhonderdvierentwintig biljard zevenhonderdzevenenzestig biljoen negenentachtig miljard honderdvijfentwintig miljoen achthonderdveertienduizendhonderdveertiende")
  , (114059301025943970552219, "honderdveertien triljard negenenvijftig triljoen driehonderdéén biljard vijfentwintig biljoen negenhonderddrieënveertig miljard negenhonderdzeventig miljoen vijfhonderdtweeënvijftigduizendtweehonderdnegentiende")
  ]
