//export default class CodeEditor extends Component {
//  render() {
//    return (
//        <MonacoEditor
//          requireConfig={{
//            url: `${appPath}/vs/loader.js`,
//            baseUrl: `${appPath}`
//          }}
//        />
//    )
//  }
//}


// Save Monaco's amd require and restore Node's require
var amdRequire = global.require;
global.require = nodeRequire;


// require node modules before loader.js comes in
var path = require('path');
function uriFromPath(_path) {
  var pathName = path.resolve(_path).replace(/\\/g, '/');
  if (pathName.length > 0 && pathName.charAt(0) !== '/') {
    pathName = '/' + pathName;
  }
  return encodeURI('file://' + pathName);
}
amdRequire.config({
  //baseUrl: uriFromPath(path.join(__dirname, '../node_modules/monaco-editor/min'))
  baseUrl: uriFromPath(path.join(__dirname, 'js'))

});
// workaround monaco-css not understanding the environment
self.module = undefined;
// workaround monaco-typescript not understanding the environment
self.process.browser = true;
amdRequire(['vs/editor/editor.main'], function () {

  monaco.languages.register({
    id: 'arm'
  });
  monaco.languages.setMonarchTokensProvider('arm', {
    // Set defaultToken to invalid to see what you do not tokenize yet
    defaultToken: 'invalid',

    ignoreCase: true,

    brackets: [
      ['{', '}', 'delimiter.curly'],
      ['[', ']', 'delimiter.square'],
      ['(', ')', 'delimiter.parenthesis'],
      ['<', '>', 'delimiter.angle']
    ],

    operators: [
      '+', '-', '*'
    ],

    keywords: [
      "ADC","ADCAL",      "ADCEQ",     "ADCGE","ADCGT","ADCHI","ADCHS","ADCLE","ADCLO","ADCLS","ADCLT","ADCMI",
      "ADCNE","ADCNV","ADCPL","ADCS","ADCSAL","ADCSEQ","ADCSGE","ADCSGT","ADCSHI","ADCSHS","ADCSLE",
      "ADCSLO","ADCSLS","ADCSLT","ADCSMI","ADCSNE","ADCSNV","ADCSPL","ADCSVC","ADCSVS","ADCVC","ADCVS","ADD",
      "ADDAL","ADDEQ","ADDGE","ADDGT","ADDHI","ADDHS","ADDLE","ADDLO","ADDLS","ADDLT","ADDMI","ADDNE","ADDNV",
      "ADDPL","ADDS","ADDSAL","ADDSEQ","ADDSGE","ADDSGT","ADDSHI","ADDSHS","ADDSLE","ADDSLO","ADDSLS","ADDSLT",
      "ADDSMI","ADDSNE","ADDSNV","ADDSPL","ADDSVC","ADDSVS","ADDVC","ADDVS","AND","ANDAL","ANDEQ","ANDGE","ANDGT",
      "ANDHI","ANDHS","ANDLE","ANDLO","ANDLS","ANDLT","ANDMI","ANDNE","ANDNV","ANDPL","ANDS","ANDSAL","ANDSEQ",
      "ANDSGE","ANDSGT","ANDSHI","ANDSHS","ANDSLE","ANDSLO","ANDSLS","ANDSLT","ANDSMI","ANDSNE","ANDSNV","ANDSPL",
      "ANDSVC","ANDSVS","ANDVC","ANDVS","ASR","ASRAL","ASREQ","ASRGE","ASRGT","ASRHI","ASRHS","ASRLE","ASRLO",
      "ASRLS","ASRLT","ASRMI","ASRNE","ASRNV","ASRPL","ASRS","ASRSAL","ASRSEQ","ASRSGE","ASRSGT","ASRSHI","ASRSHS",
      "ASRSLE","ASRSLO","ASRSLS","ASRSLT","ASRSMI","ASRSNE","ASRSNV","ASRSPL","ASRSVC","ASRSVS","ASRVC","ASRVS",
      "BIC","BICAL","BICEQ","BICGE","BICGT","BICHI","BICHS","BICLE","BICLO","BICLS","BICLT","BICMI","BICNE","BICNV",
      "BICPL","BICS","BICSAL","BICSEQ","BICSGE","BICSGT","BICSHI","BICSHS","BICSLE","BICSLO","BICSLS","BICSLT",
      "BICSMI","BICSNE","BICSNV","BICSPL","BICSVC","BICSVS","BICVC","BICVS","CMN","CMNAL","CMNEQ","CMNGE","CMNGT",
      "CMNHI","CMNHS","CMNLE","CMNLO","CMNLS","CMNLT","CMNMI","CMNNE","CMNNV","CMNPL","CMNS","CMNSAL","CMNSEQ",
      "CMNSGE","CMNSGT","CMNSHI","CMNSHS","CMNSLE","CMNSLO","CMNSLS","CMNSLT","CMNSMI","CMNSNE","CMNSNV","CMNSPL",
      "CMNSVC","CMNSVS","CMNVC","CMNVS","CMP","CMPAL","CMPEQ","CMPGE","CMPGT","CMPHI","CMPHS","CMPLE","CMPLO",
      "CMPLS","CMPLT","CMPMI","CMPNE","CMPNV","CMPPL","CMPS","CMPSAL","CMPSEQ","CMPSGE","CMPSGT","CMPSHI","CMPSHS",
      "CMPSLE","CMPSLO","CMPSLS","CMPSLT","CMPSMI","CMPSNE","CMPSNV","CMPSPL","CMPSVC","CMPSVS","CMPVC","CMPVS",
      "EOR","EORAL","EOREQ","EORGE","EORGT","EORHI","EORHS","EORLE","EORLO","EORLS","EORLT","EORMI","EORNE","EORNV",
      "EORPL","EORS","EORSAL","EORSEQ","EORSGE","EORSGT","EORSHI","EORSHS","EORSLE","EORSLO","EORSLS","EORSLT",
      "EORSMI","EORSNE","EORSNV","EORSPL","EORSVC","EORSVS","EORVC","EORVS","LSL","LSLAL","LSLEQ","LSLGE","LSLGT",
      "LSLHI","LSLHS","LSLLE","LSLLO","LSLLS","LSLLT","LSLMI","LSLNE","LSLNV","LSLPL","LSLS","LSLSAL","LSLSEQ",
      "LSLSGE","LSLSGT","LSLSHI","LSLSHS","LSLSLE","LSLSLO","LSLSLS","LSLSLT","LSLSMI","LSLSNE","LSLSNV","LSLSPL",
      "LSLSVC","LSLSVS","LSLVC","LSLVS","LSR","LSRAL","LSREQ","LSRGE","LSRGT","LSRHI","LSRHS","LSRLE","LSRLO",
      "LSRLS","LSRLT","LSRMI","LSRNE","LSRNV","LSRPL","LSRS","LSRSAL","LSRSEQ","LSRSGE","LSRSGT","LSRSHI","LSRSHS",
      "LSRSLE","LSRSLO","LSRSLS","LSRSLT","LSRSMI","LSRSNE","LSRSNV","LSRSPL","LSRSVC","LSRSVS","LSRVC","LSRVS",
      "MOV","MOVAL","MOVEQ","MOVGE","MOVGT","MOVHI","MOVHS","MOVLE","MOVLO","MOVLS","MOVLT","MOVMI","MOVNE","MOVNV",
      "MOVPL","MOVS","MOVSAL","MOVSEQ","MOVSGE","MOVSGT","MOVSHI","MOVSHS","MOVSLE","MOVSLO","MOVSLS","MOVSLT",
      "MOVSMI","MOVSNE","MOVSNV","MOVSPL","MOVSVC","MOVSVS","MOVVC","MOVVS","MVN","MVNAL","MVNEQ","MVNGE",
      "MVNGT","MVNHI","MVNHS","MVNLE","MVNLO","MVNLS","MVNLT","MVNMI","MVNNE","MVNNV","MVNPL","MVNS","MVNSAL",
      "MVNSEQ","MVNSGE","MVNSGT","MVNSHI","MVNSHS","MVNSLE","MVNSLO","MVNSLS","MVNSLT","MVNSMI","MVNSNE",
      "MVNSNV","MVNSPL","MVNSVC","MVNSVS","MVNVC","MVNVS","ORR","ORRAL","ORREQ","ORRGE","ORRGT","ORRHI",
      "ORRHS","ORRLE","ORRLO","ORRLS","ORRLT","ORRMI","ORRNE","ORRNV","ORRPL","ORRS","ORRSAL","ORRSEQ","ORRSGE",
      "ORRSGT","ORRSHI","ORRSHS","ORRSLE","ORRSLO","ORRSLS","ORRSLT","ORRSMI","ORRSNE","ORRSNV","ORRSPL","ORRSVC",
      "ORRSVS","ORRVC","ORRVS","ROR","RORAL","ROREQ","RORGE","RORGT","RORHI","RORHS","RORLE","RORLO","RORLS","RORLT",
      "RORMI","RORNE","RORNV","RORPL","RORS","RORSAL","RORSEQ","RORSGE","RORSGT","RORSHI","RORSHS","RORSLE","RORSLO",
      "RORSLS","RORSLT","RORSMI","RORSNE","RORSNV","RORSPL","RORSVC","RORSVS","RORVC","RORVS","RRX","RRXAL","RRXEQ",
      "RRXGE","RRXGT","RRXHI","RRXHS","RRXLE","RRXLO","RRXLS","RRXLT","RRXMI","RRXNE","RRXNV","RRXPL","RRXS","RRXSAL",
      "RRXSEQ","RRXSGE","RRXSGT","RRXSHI","RRXSHS","RRXSLE","RRXSLO","RRXSLS","RRXSLT","RRXSMI","RRXSNE","RRXSNV",
      "RRXSPL","RRXSVC","RRXSVS","RRXVC","RRXVS","RSB","RSBAL","RSBEQ","RSBGE","RSBGT","RSBHI","RSBHS","RSBLE",
      "RSBLO","RSBLS","RSBLT","RSBMI","RSBNE","RSBNV","RSBPL","RSBS","RSBSAL","RSBSEQ","RSBSGE","RSBSGT","RSBSHI",
      "RSBSHS","RSBSLE","RSBSLO","RSBSLS","RSBSLT","RSBSMI","RSBSNE","RSBSNV","RSBSPL","RSBSVC","RSBSVS","RSBVC",
      "RSBVS","RSC","RSCAL","RSCEQ","RSCGE","RSCGT","RSCHI","RSCHS","RSCLE","RSCLO","RSCLS","RSCLT","RSCMI","RSCNE",
      "RSCNV","RSCPL","RSCS","RSCSAL","RSCSEQ","RSCSGE","RSCSGT","RSCSHI","RSCSHS","RSCSLE","RSCSLO","RSCSLS","RSCSLT",
      "RSCSMI","RSCSNE","RSCSNV","RSCSPL","RSCSVC","RSCSVS","RSCVC","RSCVS","SBC","SBCAL","SBCEQ","SBCGE","SBCGT",
      "SBCHI","SBCHS","SBCLE","SBCLO","SBCLS","SBCLT","SBCMI","SBCNE","SBCNV","SBCPL","SBCS","SBCSAL","SBCSEQ",
      "SBCSGE","SBCSGT","SBCSHI","SBCSHS","SBCSLE","SBCSLO","SBCSLS","SBCSLT","SBCSMI","SBCSNE","SBCSNV","SBCSPL",
      "SBCSVC","SBCSVS","SBCVC","SBCVS","SUB","SUBAL","SUBEQ","SUBGE","SUBGT","SUBHI","SUBHS","SUBLE","SUBLO",
      "SUBLS","SUBLT","SUBMI","SUBNE","SUBNV","SUBPL","SUBS","SUBSAL","SUBSEQ","SUBSGE","SUBSGT","SUBSHI","SUBSHS",
      "SUBSLE","SUBSLO","SUBSLS","SUBSLT","SUBSMI","SUBSNE","SUBSNV","SUBSPL","SUBSVC","SUBSVS","SUBVC","SUBVS",
      "TEQ","TEQAL","TEQEQ","TEQGE","TEQGT","TEQHI","TEQHS","TEQLE","TEQLO","TEQLS","TEQLT","TEQMI","TEQNE","TEQNV",
      "TEQPL","TEQS","TEQSAL","TEQSEQ","TEQSGE","TEQSGT","TEQSHI","TEQSHS","TEQSLE","TEQSLO","TEQSLS","TEQSLT",
      "TEQSMI","TEQSNE","TEQSNV","TEQSPL","TEQSVC","TEQSVS","TEQVC","TEQVS","TST","TSTAL","TSTEQ","TSTGE","TSTGT",
      "TSTHI","TSTHS","TSTLE","TSTLO","TSTLS","TSTLT","TSTMI","TSTNE","TSTNV","TSTPL","TSTS","TSTSAL","TSTSEQ",
      "TSTSGE","TSTSGT","TSTSHI","TSTSHS","TSTSLE","TSTSLO","TSTSLS","TSTSLT","TSTSMI","TSTSNE","TSTSNV","TSTSPL",
      "TSTSVC","TSTSVS","TSTVC","TSTVS","B","BAL","BEQ","BGE","BGT","BHI","BHS","BL","BLAL","BLE","BLEQ","BLGE",
      "BLGT","BLHI","BLHS","BLLE","BLLO","BLLS","BLLT","BLMI","BLNE","BLNV","BLO","BLPL","BLS","BLT","BLVC","BLVS",
      "BMI","BNE","BNV","BPL","BVC","BVS","END","ENDAL","ENDEQ","ENDGE","ENDGT","ENDHI","ENDHS","ENDLE","ENDLO",
      "ENDLS","ENDLT","ENDMI","ENDNE","ENDNV","ENDPL","ENDVC","ENDVS","LDM","LDMAL","LDMB","LDMBAL","LDMBEQ","LDMBGE",
      "LDMBGT","LDMBHI","LDMBHS","LDMBLE","LDMBLO","LDMBLS","LDMBLT","LDMBMI","LDMBNE","LDMBNV","LDMBPL","LDMBVC",
      "LDMBVS","LDMDA","LDMDAAL","LDMDAEQ","LDMDAGE","LDMDAGT","LDMDAHI","LDMDAHS","LDMDALE","LDMDALO","LDMDALS",
      "LDMDALT","LDMDAMI","LDMDANE","LDMDANV","LDMDAPL","LDMDAVC","LDMDAVS","LDMDB","LDMDBAL","LDMDBEQ","LDMDBGE",
      "LDMDBGT","LDMDBHI","LDMDBHS","LDMDBLE","LDMDBLO","LDMDBLS","LDMDBLT","LDMDBMI","LDMDBNE","LDMDBNV","LDMDBPL",
      "LDMDBVC","LDMDBVS","LDMEA","LDMEAAL","LDMEAEQ","LDMEAGE","LDMEAGT","LDMEAHI","LDMEAHS","LDMEALE","LDMEALO",
      "LDMEALS","LDMEALT","LDMEAMI","LDMEANE","LDMEANV","LDMEAPL","LDMEAVC","LDMEAVS","LDMED","LDMEDAL","LDMEDEQ",
      "LDMEDGE","LDMEDGT","LDMEDHI","LDMEDHS","LDMEDLE","LDMEDLO","LDMEDLS","LDMEDLT","LDMEDMI","LDMEDNE","LDMEDNV",
      "LDMEDPL","LDMEDVC","LDMEDVS","LDMEQ","LDMFA","LDMFAAL","LDMFAEQ","LDMFAGE","LDMFAGT","LDMFAHI","LDMFAHS",
      "LDMFALE","LDMFALO","LDMFALS","LDMFALT","LDMFAMI","LDMFANE","LDMFANV","LDMFAPL","LDMFAVC","LDMFAVS","LDMFD","LDMFDAL",
      "LDMFDEQ","LDMFDGE","LDMFDGT","LDMFDHI","LDMFDHS","LDMFDLE","LDMFDLO","LDMFDLS","LDMFDLT","LDMFDMI","LDMFDNE",
      "LDMFDNV","LDMFDPL","LDMFDVC","LDMFDVS","LDMGE","LDMGT","LDMHI","LDMHS","LDMIA","LDMIAAL","LDMIAEQ","LDMIAGE",
      "LDMIAGT","LDMIAHI","LDMIAHS","LDMIALE","LDMIALO","LDMIALS","LDMIALT","LDMIAMI","LDMIANE","LDMIANV","LDMIAPL",
      "LDMIAVC","LDMIAVS","LDMIB","LDMIBAL","LDMIBEQ","LDMIBGE","LDMIBGT","LDMIBHI","LDMIBHS","LDMIBLE","LDMIBLO",
      "LDMIBLS","LDMIBLT","LDMIBMI","LDMIBNE","LDMIBNV","LDMIBPL","LDMIBVC","LDMIBVS","LDMLE","LDMLO","LDMLS","LDMLT","LDMMI",
      "LDMNE","LDMNV","LDMPL","LDMVC","LDMVS","LDR","LDRAL","LDRB","LDRBAL","LDRBEQ","LDRBGE","LDRBGT","LDRBHI",
      "LDRBHS","LDRBLE","LDRBLO","LDRBLS","LDRBLT","LDRBMI","LDRBNE","LDRBNV","LDRBPL","LDRBVC","LDRBVS","LDRDA",
      "LDRDAAL","LDRDAEQ","LDRDAGE","LDRDAGT","LDRDAHI","LDRDAHS","LDRDALE","LDRDALO","LDRDALS","LDRDALT","LDRDAMI",
      "LDRDANE","LDRDANV","LDRDAPL","LDRDAVC","LDRDAVS","LDRDB","LDRDBAL","LDRDBEQ","LDRDBGE","LDRDBGT","LDRDBHI",
      "LDRDBHS","LDRDBLE","LDRDBLO","LDRDBLS","LDRDBLT","LDRDBMI","LDRDBNE","LDRDBNV","LDRDBPL","LDRDBVC","LDRDBVS",
      "LDREA","LDREAAL","LDREAEQ","LDREAGE","LDREAGT","LDREAHI","LDREAHS","LDREALE","LDREALO","LDREALS","LDREALT",
      "LDREAMI","LDREANE","LDREANV","LDREAPL","LDREAVC","LDREAVS","LDRED","LDREDAL","LDREDEQ","LDREDGE","LDREDGT","LDREDHI",
      "LDREDHS","LDREDLE","LDREDLO","LDREDLS","LDREDLT","LDREDMI","LDREDNE","LDREDNV","LDREDPL","LDREDVC","LDREDVS","LDREQ",
      "LDRFA","LDRFAAL","LDRFAEQ","LDRFAGE","LDRFAGT","LDRFAHI","LDRFAHS","LDRFALE","LDRFALO","LDRFALS","LDRFALT","LDRFAMI",
      "LDRFANE","LDRFANV","LDRFAPL","LDRFAVC","LDRFAVS","LDRFD","LDRFDAL","LDRFDEQ","LDRFDGE","LDRFDGT","LDRFDHI","LDRFDHS",
      "LDRFDLE","LDRFDLO","LDRFDLS","LDRFDLT","LDRFDMI","LDRFDNE","LDRFDNV","LDRFDPL","LDRFDVC","LDRFDVS","LDRGE","LDRGT",
      "LDRHI","LDRHS","LDRIA","LDRIAAL","LDRIAEQ","LDRIAGE","LDRIAGT","LDRIAHI","LDRIAHS","LDRIALE","LDRIALO","LDRIALS",
      "LDRIALT","LDRIAMI","LDRIANE","LDRIANV","LDRIAPL","LDRIAVC","LDRIAVS","LDRIB","LDRIBAL","LDRIBEQ","LDRIBGE","LDRIBGT",
      "LDRIBHI","LDRIBHS","LDRIBLE","LDRIBLO","LDRIBLS","LDRIBLT","LDRIBMI","LDRIBNE","LDRIBNV","LDRIBPL","LDRIBVC","LDRIBVS",
      "LDRLE","LDRLO","LDRLS","LDRLT","LDRMI","LDRNE","LDRNV","LDRPL","LDRVC","LDRVS","STM","STMAL","STMB","STMBAL","STMBEQ",
      "STMBGE","STMBGT","STMBHI","STMBHS","STMBLE","STMBLO","STMBLS","STMBLT","STMBMI","STMBNE","STMBNV","STMBPL","STMBVC",
      "STMBVS","STMDA","STMDAAL","STMDAEQ","STMDAGE","STMDAGT","STMDAHI","STMDAHS","STMDALE","STMDALO","STMDALS","STMDALT",
      "STMDAMI","STMDANE","STMDANV","STMDAPL","STMDAVC","STMDAVS","STMDB","STMDBAL","STMDBEQ","STMDBGE","STMDBGT","STMDBHI",
      "STMDBHS","STMDBLE","STMDBLO","STMDBLS","STMDBLT","STMDBMI","STMDBNE","STMDBNV","STMDBPL","STMDBVC","STMDBVS","STMEA",
      "STMEAAL","STMEAEQ","STMEAGE","STMEAGT","STMEAHI","STMEAHS","STMEALE","STMEALO","STMEALS","STMEALT","STMEAMI","STMEANE",
      "STMEANV","STMEAPL","STMEAVC","STMEAVS","STMED","STMEDAL","STMEDEQ","STMEDGE","STMEDGT","STMEDHI","STMEDHS","STMEDLE",
      "STMEDLO","STMEDLS","STMEDLT","STMEDMI","STMEDNE","STMEDNV","STMEDPL","STMEDVC","STMEDVS","STMEQ","STMFA","STMFAAL",
      "STMFAEQ","STMFAGE","STMFAGT","STMFAHI","STMFAHS","STMFALE","STMFALO","STMFALS","STMFALT","STMFAMI","STMFANE","STMFANV",
      "STMFAPL","STMFAVC","STMFAVS","STMFD","STMFDAL","STMFDEQ","STMFDGE","STMFDGT","STMFDHI","STMFDHS","STMFDLE","STMFDLO",
      "STMFDLS","STMFDLT","STMFDMI","STMFDNE","STMFDNV","STMFDPL","STMFDVC","STMFDVS","STMGE","STMGT","STMHI","STMHS","STMIA",
      "STMIAAL","STMIAEQ","STMIAGE","STMIAGT","STMIAHI","STMIAHS","STMIALE","STMIALO","STMIALS","STMIALT","STMIAMI","STMIANE",
      "STMIANV","STMIAPL","STMIAVC","STMIAVS","STMIB","STMIBAL","STMIBEQ","STMIBGE","STMIBGT","STMIBHI","STMIBHS","STMIBLE",
      "STMIBLO","STMIBLS","STMIBLT","STMIBMI","STMIBNE","STMIBNV","STMIBPL","STMIBVC","STMIBVS","STMLE","STMLO","STMLS","STMLT",
      "STMMI","STMNE","STMNV","STMPL","STMVC","STMVS","STR","STRAL","STRB","STRBAL","STRBEQ","STRBGE","STRBGT","STRBHI","STRBHS",
      "STRBLE","STRBLO","STRBLS","STRBLT","STRBMI","STRBNE","STRBNV","STRBPL","STRBVC","STRBVS","STRDA","STRDAAL","STRDAEQ",
      "STRDAGE","STRDAGT","STRDAHI","STRDAHS","STRDALE","STRDALO","STRDALS","STRDALT","STRDAMI","STRDANE","STRDANV","STRDAPL",
      "STRDAVC","STRDAVS","STRDB","STRDBAL","STRDBEQ","STRDBGE","STRDBGT","STRDBHI","STRDBHS","STRDBLE","STRDBLO","STRDBLS",
      "STRDBLT","STRDBMI","STRDBNE","STRDBNV","STRDBPL","STRDBVC","STRDBVS","STREA","STREAAL","STREAEQ","STREAGE","STREAGT","STREAHI",
      "STREAHS","STREALE","STREALO","STREALS","STREALT","STREAMI","STREANE","STREANV","STREAPL","STREAVC","STREAVS","STRED","STREDAL",
      "STREDEQ","STREDGE","STREDGT","STREDHI","STREDHS","STREDLE","STREDLO","STREDLS","STREDLT","STREDMI","STREDNE","STREDNV","STREDPL",
      "STREDVC","STREDVS","STREQ","STRFA","STRFAAL","STRFAEQ","STRFAGE","STRFAGT","STRFAHI","STRFAHS","STRFALE","STRFALO","STRFALS",
      "STRFALT","STRFAMI","STRFANE","STRFANV","STRFAPL","STRFAVC","STRFAVS","STRFD","STRFDAL","STRFDEQ","STRFDGE","STRFDGT","STRFDHI",
      "STRFDHS","STRFDLE","STRFDLO","STRFDLS","STRFDLT","STRFDMI","STRFDNE","STRFDNV","STRFDPL","STRFDVC","STRFDVS","STRGE","STRGT",
      "STRHI","STRHS","STRIA","STRIAAL","STRIAEQ","STRIAGE","STRIAGT","STRIAHI","STRIAHS","STRIALE","STRIALO","STRIALS","STRIALT",
      "STRIAMI","STRIANE","STRIANV","STRIAPL","STRIAVC","STRIAVS","STRIB","STRIBAL","STRIBEQ","STRIBGE","STRIBGT","STRIBHI","STRIBHS",
      "STRIBLE","STRIBLO","STRIBLS","STRIBLT","STRIBMI","STRIBNE","STRIBNV","STRIBPL","STRIBVC","STRIBVS","STRLE","STRLO","STRLS",
      "STRLT","STRMI","STRNE","STRNV","STRPL","STRVC","STRVS","EQU","DCD","DCB","FILL","SPACE","ADR","ADREQ","ADRNE","ADRMI","ADRPL",
      "ADRHI","ADRHS","ADRLO","ADRLS","ADRGE","ADRGT","ADRLE","ADRLT","ADRVS","ADRVC","ADRNV","ADRAL",],

    // we include these common regular expressions
    symbols: /[=><!~?:&|+\-*\/\^%]+/,

    // C# style strings
    escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,

    // The main tokenizer for our languages
    tokenizer: {
      root: [
        // identifiers and keywords
        [/[a-z_$][\w$]*/, {
          cases: {
            '@keywords': 'keyword',
            '@default': 'identifier'
          }
        }],

        // whitespace
        { include: '@whitespace' },

        // delimiters and operators
        [/[{}()\[\]]/, '@brackets'],
        [/[<>](?!@symbols)/, '@brackets'],
        [/@symbols/, {
          cases: {
            '@operators': 'operator',
            '@default': ''
          }
        }],

        // @ annotations.
        // As an example, we emit a debugging log message on these tokens.
        // Note: message are supressed during the first load -- change some lines to see them.
        [/@\s*[a-zA-Z_\$][\w\$]*/, { token: 'annotation', log: 'annotation token: $0' }],

        // numbers
        [/#-?\d*\.\d+([eE][\-+]?\d+)?/, 'number.float'],
        [/#-?0[xX][0-9a-fA-F][0-9a-fA-F_]*/, 'number.hex'],
        [/#-?\d[\d_]*/, 'number'],
        [/-?\d*\.\d+([eE][\-+]?\d+)?/, 'number.barefloat'],
        [/-?0[xX][0-9a-fA-F][0-9a-fA-F_]*/, 'number.barehex'],
        [/-?\d[\d_]*/, 'number.bare'],

        // delimiter: after number because of .\d floats
        [/[,.]/, 'delimiter'],

        // strings
        [/"([^"\\]|\\.)*$/, 'string.invalid'],  // non-teminated string
        [/"/, { token: 'string.quote', bracket: '@open', next: '@string' }],

        // characters
        [/'[^\\']'/, 'string'],
        [/(')(@escapes)(')/, ['string', 'string.escape', 'string']],
        [/'/, 'string.invalid'],

        // ARM comments
        [/;(.*)/, 'comment'],

      ],


      string: [
        [/[^\\"]+/, 'string'],
        [/@escapes/, 'string.escape'],
        [/\\./, 'string.escape.invalid'],
        [/"/, { token: 'string.quote', bracket: '@close', next: '@pop' }]
      ],

      whitespace: [
        [/[ \t\r\n]+/, 'white'],
        //        [/\/\*/, 'comment', '@comment'],
        //        [/\/\/.*$/, 'comment'],
      ],
    }
  });

  monaco.editor.defineTheme('one-dark-pro', {
    base: 'vs-dark',
    inherit: true, // can also be false to completely replace the builtin rules
    rules: [
      { token: 'operators', foreground: '56b6c2'},
      { token: 'keywords', foreground: '56b6c2'},
      { token: 'symbols', foreground: '56b6c2'},
      { token: 'escape', foreground: '57b6c2'},
      { token: 'string', foreground: 'e06c75'}
    ],
    "colors": {
      "activityBar.background": "#2F333D",
      "activityBar.foreground": "#D7DAE0",
      "activityBarBadge.background": "#4D78CC",
      "activityBarBadge.foreground": "#F8FAFD",
      "badge.background": "#282c34",
      "button.background": "#404754",
      "debugToolBar.background": "#21252b",
      "dropdown.background": "#1d1f23",
      "diffEditor.insertedTextBackground": "#00809B33",
      "dropdown.border": "#181A1F",
      "editor.background": "#282c34",
      "editorError.foreground": "#c24038",
      "editorMarkerNavigation.background": "#21252b",
      "editorRuler.foreground": "#abb2bf26",
      "editor.lineHighlightBackground": "#2c313c",
      "editor.selectionBackground": "#67769660",
      "editor.selectionHighlightBackground": "#ffffff10",
      "editor.selectionHighlightBorder": "#ddd",
      "editorCursor.background": "#ffffffc9",
      "editorCursor.foreground": "#528bff",
      "editorBracketMatch.border": "#515a6b",
      "editorBracketMatch.background": "#515a6b",
      "editor.findMatchBackground": "#42557B",
      "editor.findMatchBorder": "#457dff",
      "editor.findMatchHighlightBackground": "#314365",
      "editor.wordHighlightBackground": "#484e5b",
      "editor.wordHighlightBorder": "#7f848e",
      "editor.wordHighlightStrongBackground": "#abb2bf26",
      "editor.wordHighlightStrongBorder": "#7f848e",
      "editorGroup.background": "#181A1F",
      "editorGroup.border": "#181A1F",
      "editorGroupHeader.tabsBackground": "#21252B",
      "editorIndentGuide.background": "#3B4048",
      "editorLineNumber.foreground": "#495162",
      "editorActiveLineNumber.foreground": "#737984",
      "editorWhitespace.foreground": "#3B4048",
      "editorHoverWidget.background": "#21252B",
      "editorHoverWidget.border": "#181A1F",
      "editorSuggestWidget.background": "#21252B",
      "editorSuggestWidget.border": "#181A1F",
      "editorSuggestWidget.selectedBackground": "#2c313a",
      "editorWidget.background": "#21252B",
      "input.background": "#1d1f23",
      "list.activeSelectionBackground": "#2c313a",
      "list.activeSelectionForeground": "#d7dae0",
      "list.focusBackground": "#383E4A",
      "list.hoverBackground": "#292d35",
      "list.highlightForeground": "#C5C5C5",
      "list.inactiveSelectionBackground": "#2c313a",
      "list.inactiveSelectionForeground": "#d7dae0",
      "peekViewEditor.matchHighlightBackground": "#29244b",
      "scrollbarSlider.background": "#4e566660",
      "scrollbarSlider.activeBackground": "#747D9180",
      "scrollbarSlider.hoverBackground": "#5A637580",
      "sideBar.background": "#21252b",
      "sideBarSectionHeader.background": "#282c34",
      "statusBar.background": "#21252B",
      "statusBar.foreground": "#9da5b4",
      "statusBarItem.hoverBackground": "#2c313a",
      "statusBar.noFolderBackground": "#21252B",
      "statusBar.debuggingBackground": "#7e0097",
      "statusBar.debuggingBorder": "#66017a",
      "statusBar.debuggingForeground": "#ffffff",
      "tab.activeBackground": "#2c313a",
      "tab.border": "#181A1F",
      "tab.inactiveBackground": "#21252B",
      "tab.hoverBackground": "#323842",
      "tab.unfocusedHoverBackground": "#323842",
      "terminal.foreground":"#C8C8C8",
      "terminal.ansiBlack": "#2D3139",
      "terminal.ansiBlue": "#2e8ccf",
      "terminal.ansiGreen": "#98c379cc",
      "terminal.ansiYellow": "#B4881D",
      "titleBar.activeBackground": "#282c34",
      "titleBar.activeForeground": "#9da5b4",
      "titleBar.inactiveBackground": "#282C34",
      "titleBar.inactiveForeground": "#6B717D",
    }
  });




  // window.code = monaco.editor.create(document.getElementById('editor'), {
   //  value: [
   //    'mov r0, #5',
   //    'mov r1, r0'
    // ].join('\n'),
    // language: 'arm',
    // theme: 'vs-light',
    // renderWhitespace: 'all',
    // roundedSelection: false,
    // scrollBeyondLastLine: false,
    // automaticLayout: true
   //});

  var mevent = new CustomEvent("monaco-ready", { "detail": "ready now!" });

  // Dispatch/Trigger/Fire the event
  document.dispatchEvent(mevent);
});





