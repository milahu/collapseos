var SORT_COLUMN_INDEX;addEvent(window,"load",sortables_init);var SORT_COLUMN_MAP=new Array,TIME_MULTIPLIERS=new Array;function sortables_init(){if(document.getElementsByTagName)for(tbls=document.getElementsByTagName("table"),ti=0;ti<tbls.length;ti++)thisTbl=tbls[ti],-1!=(" "+thisTbl.className+" ").indexOf("sortable")&&thisTbl.id&&ts_makeSortable(thisTbl)}function ts_makeSortable(t){if(t.rows&&t.rows.length>0)var e=t.rows[0];if(e){for(var r=0;r<e.cells.length;r++){var s=e.cells[r];if(-1==(" "+s.className+" ").indexOf(" nosort ")){var a=ts_getInnerText(s);s.innerHTML='<a id="sortcolid'+r+'" href="#" class="sortheader" onclick="ts_resortTable(this);return false;">'+a+'<span class="sortarrow"></span></a>',SORT_COLUMN_MAP[s.cellIndex]=r}}ts_resortTable(document.getElementById("sortcolid0"))}}function ts_getInnerText(t){if("string"==typeof t)return t;if(void 0===t)return t;if(t.innerText)return t.innerText;for(var e="",r=t.childNodes,s=r.length,a=0;a<s;a++)switch(r[a].nodeType){case 1:e+=ts_getInnerText(r[a]);break;case 3:e+=r[a].nodeValue}return e}function ts_resortTable(t){for(var e,r=0;r<t.childNodes.length;r++)t.childNodes[r].tagName&&"span"==t.childNodes[r].tagName.toLowerCase()&&(e=t.childNodes[r]);if(e){ts_getInnerText(e);var s=t.parentNode,a=SORT_COLUMN_MAP[s.cellIndex],n=getParent(s,"TABLE");if(!(n.rows.length<=1)){var o=ts_getInnerText(n.rows[1].cells[a]);sortfn=ts_sort_caseinsensitive,o.match(/^\d\d[\/-]\d\d[\/-]\d\d\d\d$/)&&(sortfn=ts_sort_date),o.match(/^\d\d[\/-]\d\d[\/-]\d\d$/)&&(sortfn=ts_sort_date),o.match(/^[\d\.]+ (second|minute|hour|day|month|year)s$/)&&(sortfn=ts_sort_time),o.match(/^[�$]/)&&(sortfn=ts_sort_currency),o.match(/^\s*[\d\.]+[kmKM\%]?$/)&&(sortfn=ts_sort_numeric),SORT_COLUMN_INDEX=a;var b=new Array,l=new Array;for(i=0;i<n.rows[0].length;i++)b[i]=n.rows[0][i];for(j=1;j<n.rows.length;j++)l[j-1]=n.rows[j];for(l.sort(sortfn),"down"==e.getAttribute("sortdir")?(ARROW="&nbsp;&nbsp;&uarr;",l.reverse(),e.setAttribute("sortdir","up")):(ARROW="&nbsp;&nbsp;&darr;",e.setAttribute("sortdir","down")),i=0;i<l.length;i++)(!l[i].className||l[i].className&&-1==l[i].className.indexOf("sortbottom"))&&n.tBodies[0].appendChild(l[i]);for(i=0;i<l.length;i++)l[i].className&&-1!=l[i].className.indexOf("sortbottom")&&n.tBodies[0].appendChild(l[i]);var d=document.getElementsByTagName("span");for(r=0;r<d.length;r++)"sortarrow"==d[r].className&&getParent(d[r],"table")==getParent(t,"table")&&(d[r].innerHTML="");e.innerHTML=ARROW}}}function getParent(t,e){return null==t?null:1==t.nodeType&&t.tagName.toLowerCase()==e.toLowerCase()?t:getParent(t.parentNode,e)}function ts_sort_date(t,e){return aa=ts_getInnerText(t.cells[SORT_COLUMN_INDEX]),bb=ts_getInnerText(e.cells[SORT_COLUMN_INDEX]),10==aa.length?dt1=aa.substr(6,4)+aa.substr(3,2)+aa.substr(0,2):(yr=aa.substr(6,2),parseInt(yr)<50?yr="20"+yr:yr="19"+yr,dt1=yr+aa.substr(3,2)+aa.substr(0,2)),10==bb.length?dt2=bb.substr(6,4)+bb.substr(3,2)+bb.substr(0,2):(yr=bb.substr(6,2),parseInt(yr)<50?yr="20"+yr:yr="19"+yr,dt2=yr+bb.substr(3,2)+bb.substr(0,2)),dt1==dt2?0:dt1<dt2?-1:1}function ts_sort_time(t,e){return aa=ts_getInnerText(t.cells[SORT_COLUMN_INDEX]),bb=ts_getInnerText(e.cells[SORT_COLUMN_INDEX]),am=1,/^[\d\.]+ (second|minute|hour|day|month|year)s$/.exec(aa)&&(am=TIME_MULTIPLIERS[RegExp.$1]),aa=parseFloat(aa)*am,isNaN(aa)&&(aa=0),bm=1,/^[\d\.]+ (second|minute|hour|day|month|year)s$/.exec(bb)&&(bm=TIME_MULTIPLIERS[RegExp.$1]),bb=parseFloat(bb)*bm,isNaN(bb)&&(bb=0),aa-bb}function ts_sort_currency(t,e){return aa=ts_getInnerText(t.cells[SORT_COLUMN_INDEX]).replace(/[^0-9.]/g,""),bb=ts_getInnerText(e.cells[SORT_COLUMN_INDEX]).replace(/[^0-9.]/g,""),parseFloat(aa)-parseFloat(bb)}function ts_sort_numeric(t,e){return at=ts_getInnerText(t.cells[SORT_COLUMN_INDEX]),aa=parseFloat(at),isNaN(aa)&&(aa=0),at.match(/[kK]$/)&&(aa*=1024),at.match(/[mM]$/)&&(aa*=1048576),bt=ts_getInnerText(e.cells[SORT_COLUMN_INDEX]),bb=parseFloat(bt),isNaN(bb)&&(bb=0),bt.match(/[kK]$/)&&(bb*=1024),bt.match(/[mM]$/)&&(bb*=1048576),aa-bb}function ts_sort_caseinsensitive(t,e){return aa=ts_getInnerText(t.cells[SORT_COLUMN_INDEX]).toLowerCase(),bb=ts_getInnerText(e.cells[SORT_COLUMN_INDEX]).toLowerCase(),aa==bb?0:aa<bb?-1:1}function ts_sort_default(t,e){return aa=ts_getInnerText(t.cells[SORT_COLUMN_INDEX]),bb=ts_getInnerText(e.cells[SORT_COLUMN_INDEX]),aa==bb?0:aa<bb?-1:1}function addEvent(t,e,r,s){return t.addEventListener?(t.addEventListener(e,r,s),!0):t.attachEvent?t.attachEvent("on"+e,r):void 0}TIME_MULTIPLIERS.second=1,TIME_MULTIPLIERS.minute=60,TIME_MULTIPLIERS.hour=3600,TIME_MULTIPLIERS.day=86400,TIME_MULTIPLIERS.year=31536e3;