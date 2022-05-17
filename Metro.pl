% Este programa encuentra la mejor manera para llegar de un punto inicial a uno final en el mÃ©tro de
% la cdmx.
% Se utiliza un modelo router para decidir la ruta.

%____________________________________________________________________________________________________
% Base de conocimientos:

% lineas existentes
linea(1). linea(2). linea(3). linea(4). linea(5). linea(6).
linea(7). linea(8). linea(9). linea(10). linea(11). linea(12).

% estaciones
estacion(observatorio). estacion(tacubaya). estacion(juanacatlan). estacion(chapultepec). 
estacion(sevilla). estacion(insurgentes). estacion(cuauhtemoc). estacion(balderas). 
estacion(saltodelagua). estacion(isabellacatolica). estacion(pinosuarez). estacion(merced). 
estacion(candelaria). estacion(sanlazaro). estacion(moctezuma). estacion(balbuena). 
estacion(blvptoaereo). estacion(gomezfarias). estacion(zaragoza). estacion(pantitlan). 
estacion(cuatrocaminos). estacion(panteones). estacion(tacuba). estacion(cuitlahuac). 
estacion(popotla). estacion(colegiomilitar). estacion(normal). estacion(sancosme).
estacion(revolucion). estacion(hidalgo). estacion(bellasartes). estacion(allende). 
estacion(zocalo). estacion(sanantonioabad). estacion(chabacano).
estacion(viaducto). estacion(xola). estacion(villadecortes). estacion(nativitas). 
estacion(portales). estacion(ermita). estacion(generalanaya). estacion(tasquenia). 
estacion(indiosverdes). estacion(deportivo18demarzo). estacion(potrero). estacion(laraza). 
estacion(tlatelolco). estacion(guerrero). estacion(juarez).
estacion(niniosheroes). estacion(hospitalgeneral). estacion(centromedico). 
estacion(etiopia). estacion(eugenia). estacion(divisiondelnorte). estacion(zapata).
estacion(coyoacan). estacion(viveros). estacion(madequevedo). estacion(copilco). 
estacion(universidad). estacion(martincarrera). estacion(talisman). estacion(bondojito).
estacion(consulado). estacion(canaldelnorte). estacion(morelos).
estacion(frayservando). estacion(jamaica). estacion(santaanita). estacion(politecnico). 
estacion(institutodelpetroleo). estacion(autobusesdelnorte). estacion(misterios). estacion(vallegomez).
estacion(eduardomolina). estacion(aragon). estacion(oceania).
estacion(terminalaerea). estacion(hangares). estacion(elrosario). estacion(tezozomoc).
estacion(azcapotzalco). estacion(ferreria). estacion(norte45). estacion(vallejo).
estacion(lindavista). estacion(lavillabasilica). estacion(aquilesserdan).
estacion(camarones). estacion(refineria). estacion(sanjoaquin).
estacion(auditorio). estacion(constituyentes). estacion(sanpedrodelospinos).
estacion(sanantonio). estacion(mixcoac). estacion(barrancadelmuerto). estacion(doctores).
estacion(garibaldi). estacion(sanjuandeletran). estacion(obrera).
estacion(laviga). estacion(coyuya).
estacion(iztacalco). estacion(apatlaco). estacion(aculco). estacion(escuadron201).
estacion(atlalilco). estacion(iztapalapa). estacion(cdelaestrella). estacion(uam1).
estacion(constitucionde1917). estacion(patriotismo). estacion(chilpancingo).
estacion(lazarocardenas). estacion(mixiuhca).
estacion(velodromo). estacion(ciudaddeportiva). estacion(puebla).
estacion(agricolaoriental). estacion(canaldesanjuan). estacion(tepalcates).
estacion(guelatao). estacion(penionviejo). estacion(acatitla). estacion(santamarta).
estacion(losreyes). estacion(lapaz). estacion(buenavista). estacion(lagunilla).
estacion(tepito). estacion(rfloresmagon). estacion(zapotitlan).
estacion(rrubio). estacion(deportivooceania). estacion(villadearagon).
estacion(bosquedearagon). estacion(nezahualcoyotl). estacion(impulsora). estacion(riodelosremedios). 
estacion(muzquiz). estacion(tecnologico). estacion(olimpica). estacion(plazaaragon).
estacion(ciudadazteca). estacion(insurgentessur). estacion(hospital20denoviembre).
estacion(parquedelosvenados). estacion(ejecentral). estacion(mexicaltzingo). 
estacion(culhuacan). estacion(sanandrestomatlan). estacion(lomasestrella).
estacion(calle11). estacion(perifericooriente). estacion(tezonco). estacion(olivos).
estacion(nopalera). estacion(tlaltengo). estacion(tlahuac). estacion(polanco).

% estructura general
% estos hechos, expresados de la forma pertenece(ESTACION,LINEA)
% indican que la ESTACION pertenece a la LINEA
% nota: una ESTACION puede pretenecer a mas de una linea
% 	hacer la peticion pertenece(ejemplo,N) devolvera solamente el primer resultado encontrado

% estaciones de la linea 1
pertenece(observatorio,1). pertenece(tacubaya,1). pertenece(juanacatlan,1). pertenece(chapultepec,1). 
pertenece(sevilla,1). pertenece(insurgentes,1). pertenece(cuauhtemoc,1). pertenece(balderas,1). 
pertenece(saltodelagua,1). pertenece(isabellacatolica,1). pertenece(pinosuarez,1). pertenece(merced,1). 
pertenece(candelaria,1). pertenece(sanlazaro,1). pertenece(moctezuma,1). pertenece(balbuena,1). 
pertenece(blvptoaereo,1). pertenece(gomezfarias,1). pertenece(zaragoza,1). pertenece(pantitlan,1). 

% estaciones de la linea 2
pertenece(cuatrocaminos,2). pertenece(panteones,2). pertenece(tacuba,2). pertenece(cuitlahuac,2). 
pertenece(popotla,2). pertenece(colegiomilitar,2). pertenece(normal,2). pertenece(sancosme,2). 
pertenece(revolucion,2). pertenece(hidalgo,2). pertenece(bellasartes,2). pertenece(allende,2). 
pertenece(zocalo,2). pertenece(pinosuarez,2). pertenece(sanantonioabad,2). pertenece(chabacano,2). 
pertenece(viaducto,2). pertenece(xola,2). pertenece(villadecortes,2). pertenece(nativitas,2). 
pertenece(portales,2). pertenece(ermita,2). pertenece(generalanaya,2). pertenece(tasquenia,2).

% estaciones de la linea 3
pertenece(indiosverdes,3). pertenece(deportivo18demarzo,3). pertenece(potrero,3). pertenece(laraza,3). 
pertenece(tlatelolco,3). pertenece(guerrero,3). pertenece(hidalgo,3). pertenece(juarez,3). 
pertenece(balderas,3). pertenece(niniosheroes,3). pertenece(hospitalgeneral,3). pertenece(centromedico,3). 
pertenece(etiopia,3). pertenece(eugenia,3). pertenece(divisiondelnorte,3). pertenece(zapata,3). 
pertenece(coyoacan,3). pertenece(viveros,3). pertenece(madequevedo,2). pertenece(copilco,3). 
pertenece(universidad,3). 

% estaciones de la linea 4
pertenece(martincarrera,4). pertenece(talisman,4). pertenece(bondojito,4). pertenece(consulado,4). 
pertenece(canaldelnorte,4). pertenece(morelos,4). pertenece(candelaria,4). pertenece(frayservando,4). 
pertenece(jamaica,4). pertenece(santaanita,4). 

% estaciones de la linea 5
pertenece(politecnico,5). pertenece(institutodelpetroleo,5). pertenece(autobusesdelnorte,5). pertenece(laraza,5). 
pertenece(misterios,5). pertenece(vallegomez,5). pertenece(consulado,5). pertenece(eduardomolina,5). 
pertenece(aragon,5). pertenece(oceania,5). pertenece(terminalaerea,5). pertenece(hangares,5). 
pertenece(pantitlan,5). 

% estaciones de la linea 6
pertenece(elrosario,6). pertenece(tezozomoc,6). pertenece(azcapotzalco,6). pertenece(ferreria,6). 
pertenece(norte45,6). pertenece(vallejo,6). pertenece(institutodelpetroleo,6). pertenece(lindavista,6). 
pertenece(deportivo18demarzo,6). pertenece(lavillabasilica,3). pertenece(martincarrera,3). 

% estaciones de la linea 7
pertenece(elrosario,7). pertenece(aquilesserdan,7). pertenece(camarones,7). pertenece(refineria,7). 
pertenece(tacuba,7). pertenece(sanjoaquin,7). pertenece(polanco,7). pertenece(auditorio,7). pertenece(constituyentes,7). 
pertenece(tacubaya,7). pertenece(sanpedrodelospinos,7). pertenece(sanantonio,7). pertenece(mixcoac,7). 
pertenece(barrancadelmuerto,7). 

% estaciones de la linea 8
pertenece(garibaldi,8). pertenece(bellasartes,8). pertenece(sanjuandeletran,8). pertenece(saltodelagua,8). 
pertenece(doctores,8). pertenece(obrera,8). pertenece(chabacano,8). pertenece(laviga,8). 
pertenece(santaanita,8). pertenece(coyuya,8). pertenece(iztacalco,8). pertenece(apatlaco,8). 
pertenece(aculco,8). pertenece(escuadron201,8). pertenece(atlalilco,8). pertenece(iztapalapa,8). 
pertenece(cdelaestrella,8). pertenece(uam1,8). pertenece(constitucionde1917,8). 

% estaciones de la linea 9
pertenece(tacubaya,9). pertenece(patriotismo,9). pertenece(chilpancingo,9). pertenece(centromedico,9). 
pertenece(lazarocardenas,9). pertenece(chabacano,9). pertenece(jamaica,9). pertenece(mixiuhca,9). 
pertenece(velodromo,9). pertenece(ciudaddeportiva,9). pertenece(puebla,9). pertenece(pantitlan,9). 

% estaciones de la linea a
pertenece(pantitlan,10). pertenece(agricolaoriental,10). pertenece(canaldesanjuan,10). pertenece(tepalcates,10). 
pertenece(guelatao,10). pertenece(penionviejo,10). pertenece(acatitla,10). pertenece(santamarta,10). 
pertenece(losreyes,10). pertenece(lapaz,10). 

% estaciones de la linea b
pertenece(buenavista,11). pertenece(guerrero,11). pertenece(garibaldi,11). pertenece(lagunilla,11). 
pertenece(tepito,11). pertenece(morelos,11). pertenece(sanlazaro,11). pertenece(rfloresmagon,11). 
pertenece(rrubio,11). pertenece(oceania,11). pertenece(deportivooceania,11). pertenece(bosquedearagon,11). 
pertenece(villadearagon,11). pertenece(nezahualcoyotl,11). pertenece(impulsora,11). pertenece(riodelosremedios,11). 
pertenece(muzquiz,11). pertenece(tecnologico,11). pertenece(ecatepec,11). pertenece(olimpica,11). pertenece(plazaaragon,11). 
pertenece(ciudadazteca,11).

% estaciones de la linea 12
pertenece(mixcoac,12). pertenece(insurgentessur,12). pertenece(hospital20denoviembre,12). pertenece(zapata,12). 
pertenece(parquedelosvenados,12). pertenece(ejecentral,12). pertenece(ermita,12). pertenece(mexicaltzingo,12). 
pertenece(atlalilco,12). pertenece(culhuacan,12). pertenece(sanandrestomatlan,12). pertenece(lomasestrella,12). 
pertenece(calle11,12). pertenece(perifericooriente,12). pertenece(tezonco,12). pertenece(olivos,12). 
pertenece(nopalera,12). pertenece(zapotitlan,12). pertenece(tlaltengo,12). pertenece(tlahuac,12).

% adyacente:
% describe las estaciones que estÃ¡n adyacentes sobre cada linea
% nota: los predicados sÃ­ dependen del orden en que se escriben las estaciones

% linea 1
adyacente(observatorio, tacubaya, 1). adyacente(tacubaya, juanacatlan,1). adyacente(juanacatlan, chapultepec,1).
adyacente(chapultepec, sevilla,1). adyacente(sevilla, insurgentes,1). adyacente(insurgentes, cuauhtemoc,1). 
adyacente(cuauhtemoc, balderas, 1). adyacente(balderas, saltodelagua,1). adyacente(saltodelagua, isabellacatolica,1).
adyacente(isabellacatolica, pinosuarez,1). adyacente(pinosuarez, merced,1). adyacente(merced, candelaria,1).
adyacente(candelaria, sanlazaro, 1). adyacente(sanlazaro, moctezuma,1). adyacente(moctezuma, balbuena,1).
adyacente(balbuena, blvptoaereo,1). adyacente(blvptoaereo, gomezfarias,1). adyacente(gomezfarias, zaragoza,1). adyacente(zaragoza,pantitlan, 1).
% linea 2
adyacente(cuatrocaminos,panteones,2). adyacente(panteones,tacuba,2). adyacente(tacuba,cuitlahuac,2). adyacente(cuitlahuac,popotla,2).
adyacente(popotla,colegiomilitar,2). adyacente(colegiomilitar,normal,2). adyacente(normal,sancosme,2). adyacente(sancosme,revolucion,2). 
adyacente(revolucion,hidalgo,2). adyacente(hidalgo,bellasartes,2). adyacente(bellasartes,allende,2). adyacente(allende,zocalo,2). 
adyacente(zocalo,pinosuarez,2). adyacente(pinosuarez,sanantonioabad,2). adyacente(sanantonioabad,chabacano,2). adyacente(chabacano,viaducto,2). 
adyacente(viaducto,xola,2). adyacente(xola,villadecortes,2). adyacente(villadecortes,nativitas,2). adyacente(nativitas,portales,2). 
adyacente(portales,ermita,2). adyacente(ermita,generalanaya,2). adyacente(generalanaya,tasquenia,2). 
% linea 3
adyacente(indiosverdes,deportivo18demarzo,3). adyacente(deportivo18demarzo,potrero,3). adyacente(potrero,laraza,3). adyacente(laraza,tlatelolco,3). 
adyacente(tlatelolco,guerrero,3). adyacente(guerrero, hidalgo,3). adyacente(hidalgo,juarez,3). adyacente(juarez,balderas,3). adyacente(balderas,niniosheroes,3). 
adyacente(niniosheroes,hospitalgeneral,3). adyacente(hospitalgeneral,centromedico,3). adyacente(centromedico,etiopia,3). adyacente(etiopia,eugenia,3). 
adyacente(eugenia,divisiondelnorte,3). adyacente(divisiondelnorte,zapata,3). adyacente(zapata,coyoacan,3). adyacente(coyoacan,viveros,3). 
adyacente(viveros,madequevedo,3). adyacente(madequevedo,copilco,3). adyacente(copilco,universidad,3). 
% linea 4
adyacente(martincarrera,talisman,4). adyacente(talisman,bondojito,4). adyacente(bondojito,consulado,4). adyacente(consulado,canaldelnorte,4). 
adyacente(canaldelnorte,morelos,4). adyacente(morelos,candelaria,4). adyacente(candelaria,frayservando,4). adyacente(frayservando,jamaica,4). 
adyacente(jamaica,santaanita,4). 
% linea 5
adyacente(pantitlan,hangares,5). adyacente(hangares,terminalaerea,5). adyacente(terminalaerea,oceania,5). adyacente(oceania,aragon,5). 
adyacente(aragon,eduardomolina,5). adyacente(eduardomolina,consulado,5). adyacente(consulado,vallegomez,5). adyacente(vallegomez,misterios,5). 
adyacente(misterios,laraza,5). adyacente(laraza,autobusesdelnorte,5). adyacente(autobusesdelnorte,institutodelpetroleo,5). adyacente(institutodelpetroleo,politecnico,5). 
% linea 6
adyacente(elrosario,tezozomoc,6). adyacente(tezozomoc,azcapotzalco,6). adyacente(azcapotzalco,ferreria,6). adyacente(ferreria,norte45,6).
adyacente(norte45,vallejo,6). adyacente(vallejo,institutodelpetroleo,6). adyacente(institutodelpetroleo,lindavista,6). 
adyacente(lindavista,deportivo18demarzo,6).  adyacente(deportivo18demarzo,lavillabasilica,6). adyacente(lavillabasilica,martincarrera,6). 
% linea 7
adyacente(elrosario,aquilesserdan,7). adyacente(aquilesserdan,camarones,7). adyacente(camarones,refineria,7). adyacente(refineria,tacuba,7). 
adyacente(tacuba,sanjoaquin,7). adyacente(sanjoaquin,polanco,7). adyacente(polanco,auditorio,7). adyacente(auditorio,constituyentes,7). 
adyacente(constituyentes,tacubaya,7). adyacente(tacubaya,sanpedrodelospinos,7). adyacente(sanpedrodelospinos,sanantonio,7). 
adyacente(sanantonio,mixcoac,7). adyacente(mixcoac,barrancadelmuerto,7). 
% linea 8
adyacente(garibaldi,bellasartes,8). adyacente(bellasartes,sanjuandeletran,8). adyacente(sanjuandeletran,saltodelagua,8). adyacente(saltodelagua,doctores,8). 
adyacente(doctores,obrera,8). adyacente(obrera,chabacano,8). adyacente(chabacano,laviga,8). adyacente(laviga,santaanita,8). adyacente(santaanita,coyuya,8). 
adyacente(coyuya,iztacalco,8). adyacente(iztacalco,apatlaco,8). adyacente(apatlaco,aculco,8). adyacente(aculco,escuadron201,8). 
adyacente(escuadron201,atlalilco,8). adyacente(atlalilco,iztapalapa,8). adyacente(iztapalapa,cdelaestrella,8). adyacente(cdelaestrella,uam1,8). 
adyacente(uam1,constitucionde1917,8). 
% linea 9
adyacente(tacubaya,patriotismo,9). adyacente(patriotismo,chilpancingo,9). adyacente(chilpancingo,centromedico,9). adyacente(centromedico,lazarocardenas,9). 
adyacente(lazarocardenas,chabacano,9). adyacente(chabacano,jamaica,9). adyacente(jamaica,mixiuhca,9). adyacente(mixiuhca,velodromo,9). 
adyacente(velodromo,ciudaddeportiva,9). adyacente(ciudaddeportiva,puebla,9). adyacente(puebla,pantitlan,9). 
% linea a (10)
adyacente(pantitlan,10,agricolaoriental,10). adyacente(agricolaoriental,canaldesanjuan,10). adyacente(canaldesanjuan,tepalcates,10). 
adyacente(tepalcates,guelatao,10). adyacente(guelatao,penionviejo,10). adyacente(penionviejo,10,acatitla,10). adyacente(acatitla,santamarta,10). 
adyacente(santamarta,losreyes,10). adyacente(losreyes,lapaz,10). 
% linea b (11)
adyacente(ciudadazteca,plazaaragon,11). adyacente(plazaaragon,olimpica,11). adyacente(olimpica,ecatepec,11). adyacente(ecatepec,muzquiz,11). 
adyacente(muzquiz,riodelosremedios,11). adyacente(riodelosremedios,impulsora,11). adyacente(impulsora,nezahualcoyotl,11). adyacente(nezahualcoyotl,villadearagon,11). 
adyacente(villadearagon,bosquedearagon,11). adyacente(bosquedearagon,deportivooceania,11).  adyacente(deportivooceania,oceania,11). 
adyacente(oceania,rrubio,11). adyacente(rrubio,rfloresmagon,11). adyacente(rfloresmagon,sanlazaro,11). adyacente(sanlazaro,morelos,11). 
adyacente(morelos,tepito,11). adyacente(tepito,lagunilla,11). adyacente(lagunilla,garibaldi,11). adyacente(garibaldi,guerrero,11). adyacente(guerrero,buenavista,11). 
% linea 12
adyacente(mixcoac,insurgentessur,12). adyacente(insurgentessur,hospital20denoviembre,12). adyacente(hospital20denoviembre,zapata,12). adyacente(zapata,parquedelosvenados,12). 
adyacente(parquedelosvenados,ejecentral,12). adyacente(ejecentral,ermita,12). adyacente(ermita,mexicaltzingo,12). adyacente(mexicaltzingo,atlalilco,12). 
adyacente(atlalilco,culhuacan,12). adyacente(culhuacan,sanandrestomatlan,12). adyacente(sanandrestomatlan,lomasestrella,12). adyacente(lomasestrella,calle11,12). 
adyacente(calle11,perifericooriente,12). adyacente(perifericooriente,tezonco,12). adyacente(tezonco,olivos,12). adyacente(olivos,nopalera,12).
adyacente(nopalera,zapotitlan,12). adyacente(zapotitlan,tlaltengo,12). adyacente(tlaltengo,tlahuac,12).

%____________________________________________________________________________________________________
% Casos:
% El modelo 'router' toma en cuenta casos que se pueden generalizar para llegar a soluciones particulares



%____________________________________________________________________________________________________


% Predicado que relaciona a una estacion y las lineas que contecta (si las hay).
% conexion(i,i,o)
% conexion(i,o,i)
% conexion(o,i,i)
conexion(ESTACION,LINEA1,LINEA2):-
	linea(LINEA1),
	linea(LINEA2),
	not(LINEA1 is LINEA2),
	pertenece(ESTACION,LINEA1),
	pertenece(ESTACION,LINEA2).

%____________________________________________________________________________________________________




% predicado que describe un viaje entre cualquier par de estaciones en una misma linea
% estÃ¡ dividido en dos 'sub-predicados' (viajelinea y viajelineai) que checan ambas direcciones.

% viajelinea(i,i,i)
% viajelinea(i,i,o) *nota: esta variante idealmente solamente se usarÃ­a si no hay ambiguedad en los posibles resultados de la salida
viajelinea(E1,E2,LINEA):-
	pertenece(E1,LINEA),
	pertenece(E2,LINEA),
	write("Linea "), write(LINEA), write(": "),
	viajelinea(E1,E2,[],LINEA).
viajelinea(E1,E2,LINEA):-
	pertenece(E1,LINEA),
	pertenece(E2,LINEA),
	viajelineai(E1,E2,[],LINEA).
viajelinea(E1,E2,SECUENCIA,LINEA):-
	pertenece(E1,LINEA),
	pertenece(E2,LINEA),
	adyacente(E1,E2,LINEA),
	append(SECUENCIA,[E1],SACT),
	append(SACT,[E2],SACT2),
	write(SACT2),nl,!.
viajelinea(E1,E2,SECUENCIA,LINEA):-
	pertenece(E1,LINEA),
	pertenece(E2,LINEA),
	adyacente(E1,SIGUIENTE,LINEA),
	append(SECUENCIA,[E1],SACT),
	viajelinea(SIGUIENTE,E2,SACT,LINEA).
viajelineai(E2,E1,SECUENCIA,LINEA):-
	pertenece(E1,LINEA),
	pertenece(E2,LINEA),
	adyacente(E1,E2,LINEA),
	append(SECUENCIA,[E2],SACT),
	append(SACT,[E1],SACT2),
	write(SACT2),nl,!.
viajelineai(E2,E1,SECUENCIA,LINEA):-
	pertenece(E1,LINEA),
	pertenece(E2,LINEA),
	adyacente(ANTERIOR,E2,LINEA),
	append(SECUENCIA,[E2],SACT),
	viajelineai(ANTERIOR,E1,SACT,LINEA).

%____________________________________________________________________________________________________

% predicado que describe el transbordo de una linea a otra indicando las estaciones de inicio y destino
% Las lineas de inicio y destino deben tener,al menos, una estacion de transbordo en comun

transbordo(EINICIAL,EMETA):-
	pertenece(EINICIAL,LINICIAL),
	pertenece(EMETA,LMETA),
	conexion(ETRANSBORDO,LINICIAL,LMETA),
	viajelinea(EINICIAL,ETRANSBORDO,LINICIAL),
	viajelinea(ETRANSBORDO,EMETA,LMETA).


% sobrecarga que indica una linea especifica por la cual se desee llegar a la estacion de meta
% esto es en caso de que la estacion de meta pertenezca a mas de una linea
transbordo(EINICIAL,EMETA,LPREFERENCIA):-
	pertenece(EINICIAL,LINICIAL),
	pertenece(EMETA,LPREFERENCIA),
	conexion(ETRANSBORDO,LINICIAL,LPREFERENCIA),
	viajelinea(EINICIAL,ETRANSBORDO,LINICIAL),
	viajelinea(ETRANSBORDO,EMETA,LPREFERENCIA).

%____________________________________________________________________________________________________

% nota: falta solucionar error donde numeros y letras no son comparables en la funcion 'conexion'

posibleconexion(LINEAINI,LINEAFIN):-
	conexion(_,LINEAINI,LINEAFIN),
	write(LINEAFIN),nl.
posibleconexion(LINEAINI,LINEAFIN):-
	conexion(_,LINEAINI,TRANSBORDO),
	write(LINEAINI), write(" - "),
	posibleconexion(TRANSBORDO,LINEAFIN).




