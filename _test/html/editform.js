let fieldChanging = false;
let imgMnu = null;
let fileMnu = null;
let dupMnu = null;

function getDataStateDS() {
	return document.getElementById('datastate').dataset;
}

function getFreshValue() {
	return document.getElementById('datastate').dataset.fresh;
}

function setActivePage(pg, ti) {
	let tabs = pg.childNodes[0];
	let tabsArea = pg.children[1];
	let i = parseInt(pg.dataset.tabindex);
	
	if (i >= 0) {
		tabs.children[i].className = '';
		tabsArea.children[i].style.display = 'none';
	}
	
	let tab = tabs.children[ti];
	if (tab) {
		tab.className = 'sel';
		if (tab.offsetLeft + tab.offsetWidth > tabs.scrollLeft + tabs.offsetWidth) {
			tab.scrollIntoView({inline: "center"});
		} else if (tab.offsetLeft < tabs.scrollLeft) {
			tab.scrollIntoView({inline: "center"});
		}
		tabsArea.childNodes[ti].style.display = '';
	}
	pg.dataset.tabindex = ti;
}

function pagecontrol_tabclick(pgid, ti) {
	setActivePage( document.getElementById(pgid), ti );
	
	SendRequest('POST', getCurrentUrl() + '&setpage', 'pgid=' + pgid + '&index=' + ti + '&fresh=' + getFreshValue(), (Request) => {
		if (Request.status != rcAjaxOk) {
			showAjaxError(Request);
		}
	});
}

function goBack() {
	let url = null;
	if (document.referrer != '') url = new URL(document.referrer);
	
	showCurtain();
	
	if (url == null || url.pathname.toLowerCase() != location.pathname.toLowerCase() || window.history.length == 1) {
		let params = new URLSearchParams(getCurrentUrl().slice(1));
		if (params.has('fm')) {
			location.href = '?fm=' + params.get('fm');
		}
	} else {
		window.history.back();
	}
}

function getParamsLength(params) {
	if (params.size) 
		return params.size
	else {
		let i = 0;
		for (let k of params.keys()) i++;
		return i;
	}
}

function goUrl(url, gotoOption) {
	if (gotoOption != gtoNewTab) showCurtain();
	if (url == 'back') goBack() 
	else if (url == getCurrentUrl) reloadPage();
	else if (gotoOption == gtoDefault) location.href = url
	else if (gotoOption == gtoReplaceUrl) location.replace(url)
	else if (gotoOption == gtoNewTab) window.open(url, '_blank');
}	

function showFileMenu(cid) { 
	if (fileMnu && fileMnu.cid == cid) {
		fileMnu.hide();
		return;
	}
	fileMnu = new FileMenu(cid, getFreshValue());
	fileMnu.show();
	fileMnu.onChange = function(json) {
		processJson(json);
	}
	fileMnu.onHide = function(that) {
		if (that == fileMnu) fileMnu = null;
	}
}

function showImageMenu(cid) { 
	if (event.target.hasAttribute('disabled')) return;
	
	if (imgMnu && imgMnu.cid == cid) {
		imgMnu.hide();
		return;
	}
	imgMnu = new ImageMenu(cid, getFreshValue());
	imgMnu.show();
	imgMnu.onChange = function(json) {
		processJson(json);
	}
	imgMnu.onHide = function(that) {
		if (that == imgMnu) imgMnu = null;
	}
}

/*function getElementsByAttr(tagName, attrName) {
  let matchingElements = [];
  let allElements = document.getElementsByTagName(tagName);
  for (let i = 0, n = allElements.length; i < n; i++)
  {
    if (allElements[i].hasAttribute(attrName))
    {
      matchingElements.push(allElements[i]);
    }
  }
  return matchingElements;
}*/

function processJson(jsonRoot) {
	if (jsonRoot.gotoUrl) {
		goUrl(jsonRoot.gotoUrl, jsonRoot.gotoOption);
		return;
	}
	
	for (jsonObj of jsonRoot.fields) {
		let e = document.getElementById(jsonObj.field);
		if (e) {
			if (jsonObj.key) {
				e.previousSibling.value = jsonObj.key;
			}
			e.value = jsonObj.value;
			if (e.type == 'checkbox') {
				e.checked = e.value == 1;
				e.indeterminate = e.value == '';
			}
		}
	}
	for (jsonObj of jsonRoot.images) {
		let e = document.getElementById(jsonObj.field);
		if (e) {
			if (e.src != '') e.src = jsonObj.src
			else e.removeAttribute('src');
			
		}
	}
	/*if (jsonRoot.labels.length > 0) {
		//let labels = getElementsByAttr('span', 'fieldname');
		let labels = document.querySelectorAll('span[fieldname]');
		let lbl;
		for (jsonObj of jsonRoot.labels) {
			lbl = findElementByAttrValue(labels, 'fieldname', jsonObj.fieldName);
			if (lbl) {
				lbl.innerHTML = jsonObj.value;
			}
		}
	}*/
	/* Используем изменение свойства caption
	if (jsonRoot.labels.length > 0) {
		let lbl;
		for (jsonObj of jsonRoot.labels) {
			lbl = document.querySelector('span[fieldname="' + jsonObj.fieldName + '"]');
			if (lbl) {
				if (lbl.children.length == 0) lbl.innerHTML = jsonObj.value
				else lbl.children[0].innerHTML = jsonObj.value;
			}
		}
	}*/
	for (jsonObj of jsonRoot.props) {
		let ctrl = document.getElementById(jsonObj.name);
		if (!ctrl) continue;
		if (jsonObj.prop == 'visible') {
			switch (jsonObj.type) {
				case 'tdxedit':
				case 'tdxcalcedit':
				case 'tdxcounter':
				case 'tdxmemo':
				case 'tdxbutton':
				case 'tdxobjectfield':
				case 'tdximage':
				case 'tdxdbimage':
				case 'tdxlabel':
				case 'tdxshape':
				case 'tdxpivotgrid':
				case 'tdxpagecontrol':
				case 'tdxchart':
					ctrl.style.display = (jsonObj.value == '1' ? '' : 'none');
					break;
				case 'tdxdateedit':
				case 'tdxtimeedit':
				case 'tdxcombobox':
				case 'tdxlookupcombobox':
				case 'tdxfile':
					ctrl.style.display = (jsonObj.value == '1' ? '' : 'none');
					ctrl.nextSibling.style.display = ctrl.style.display;
					break;
				case 'tdxcheckbox':
					ctrl.parentElement.style.display = (jsonObj.value == '1' ? '' : 'none');
					break;
				case 'tdxgrid':
				case 'tdxquerygrid':
					ctrl.style.display = (jsonObj.value == '1' ? '' : 'none');
					if (ctrl.previousSibling.classList.contains('gridcmd'))
						ctrl.previousSibling.style.display = ctrl.style.display;
					break;
				case 'tdxgroupbox':
					ctrl.style.display = (jsonObj.value == '1' ? '' : 'none');
					break;
				case 'tdxtabsheet':
					let pg = ctrl.closest('div.pages');
					let tabs = pg.children[0];
					let tabsArea = pg.children[1];
					let i = indexNodeOf(tabsArea, ctrl);
					tabs.children[i].style.display = (jsonObj.value == '1' ? '' : 'none');
					break;
			}
		} else if (jsonObj.prop == 'enabled') {
			let disabled = (jsonObj.value == '0');
			switch (jsonObj.type) {
				case 'tdxedit':
				case 'tdxcalcedit':
				case 'tdxcounter':
				case 'tdxmemo':
				case 'tdxbutton':
				case 'tdxobjectfield':
				case 'tdximage':
				case 'tdxdbimage':
				case 'tdxlabel':
				case 'tdxshape':
				case 'tdxpivotgrid':
				case 'tdxpagecontrol':
				case 'tdxgroupbox':
				case 'tdxcheckbox':
					if (disabled)
						ctrl.setAttribute('disabled', '')
					else
						ctrl.removeAttribute('disabled');
					break;
				case 'tdxdateedit':
				case 'tdxtimeedit':
				case 'tdxcombobox':
				case 'tdxlookupcombobox':
				case 'tdxfile':
					if (disabled) {
						ctrl.setAttribute('disabled', '');
						ctrl.nextSibling.setAttribute('disabled', '');
					} else {
						ctrl.removeAttribute('disabled');
						ctrl.nextSibling.removeAttribute('disabled');
					}
					break;
				case 'tdxgrid':
				case 'tdxquerygrid':
					if (disabled) {
						ctrl.setAttribute('disabled', '');
						if (ctrl.previousSibling && ctrl.previousSibling.classList.contains('gridcmd'))
							ctrl.previousSibling.children[0].children[0].setAttribute('disabled', '')
						else {
							let btns = ctrl.querySelectorAll('.gridbns-up button, .gridbns-down button');
							if (btns)
								for (let btn of btns) {
									btn.setAttribute('disabled', '');
								}
						}
					} else {
						ctrl.removeAttribute('disabled');
						if (ctrl.previousSibling && ctrl.previousSibling.classList.contains('gridcmd'))
							ctrl.previousSibling.children[0].children[0].removeAttribute('disabled')
						else {
							let btns = ctrl.querySelectorAll('.gridbns-up button, .gridbns-down button');
							if (btns) 
								for (let btn of btns) {
									btn.removeAttribute('disabled');
								}
						}
					}
					break;
			}
		} else if (jsonObj.prop == 'color') {
			switch (jsonObj.type) {
				case 'tdxedit':
				case 'tdxcalcedit':
				case 'tdxcounter':
				case 'tdxmemo':
				case 'tdxobjectfield':
				case 'tdxlabel':
				case 'tdxshape':
				case 'tdxdateedit':
				case 'tdxtimeedit':
				case 'tdxcombobox':
				case 'tdxlookupcombobox':
				case 'tdxfile':
					ctrl.style.backgroundColor = jsonObj.value;
					break;
				case 'tdxcheckbox':
					ctrl.parentElement.style.backgroundColor = jsonObj.value;
					break;
			}
		} else if (jsonObj.prop == 'font') {
			if (jsonObj.type == 'tdxcheckbox') ctrl = ctrl.parentElement
			else if (jsonObj.type == 'tdxgroupbox') ctrl = ctrl.querySelector('.groupcap');
			switch (jsonObj.type) {
				case 'tdxcheckbox':
				case 'tdxedit':
				case 'tdxcalcedit':
				case 'tdxcounter':
				case 'tdxmemo':
				case 'tdxobjectfield':
				case 'tdxlabel':
				case 'tdxdateedit':
				case 'tdxtimeedit':
				case 'tdxcombobox':
				case 'tdxlookupcombobox':
				case 'tdxfile':
				case 'tdxgrid':
				case 'tdxquerygrid':
				case 'tdxgroupbox':
				case 'tdxpagecontrol':
					let parts = jsonObj.value.split(';');
					ctrl.style.fontFamily = parts[0];
					ctrl.style.fontSize = parts[1] + 'px';
					if (parts[2].includes('b')) ctrl.style.fontWeight = 'bold'
					else ctrl.style.fontWeight = 'normal';
					if (parts[2].includes('i')) ctrl.style.fontStyle = 'italic'
					else ctrl.style.fontStyle = 'normal';
					let textDecor = '';
					if (parts[2].includes('u')) textDecor = 'underline';
					if (parts[2].includes('u')) textDecor += ' line-through';
					if (textDecor == '') textDecor = 'none';
					ctrl.style.textDecoration = textDecor;
					ctrl.style.color = parts[3];
					break;
			}
			if (jsonObj.type == 'tdxgroupbox') {
				let top = parseInt(ctrl.style.fontSize) * 0.6;
				ctrl.parentElement.style.top = top + 'px';
				ctrl.parentElement.style.height = 'calc(100% - ' + top + 'px)';
				
				top = parseInt(ctrl.style.fontSize) * 0.7;
				ctrl.nextSibling.style.top = top + 'px';
				ctrl.nextSibling.style.height = 'calc(100% - ' + top + 'px)';
			} else if (jsonObj.type == 'tdxpagecontrol') {
				let top = parseInt(ctrl.style.fontSize) * 1.5;
				let tabsArea = ctrl.children[1];
				tabsArea.style.top = top + 'px';
				tabsArea.style.height = 'calc(100% - ' + top + 'px)';
			}
		} else if (jsonObj.prop == 'bounds') {
			let parts = jsonObj.value.split(';');
			let x = parseInt(parts[0]);
			let y = parseInt(parts[1]);
			let w = parseInt(parts[2]);
			let h = parseInt(parts[3]);
			if (jsonObj.type == 'tdxcheckbox') ctrl = ctrl.parentElement;
			switch (jsonObj.type) {
				case 'tdxedit':
				case 'tdxcalcedit':
				case 'tdxcounter':
				case 'tdxmemo':
				case 'tdxbutton':
				case 'tdxobjectfield':
				case 'tdximage':
				case 'tdxdbimage':
				case 'tdxlabel':
				case 'tdxshape':
				case 'tdxpivotgrid':
				case 'tdxcheckbox':
				case 'tdximage':
				case 'tdxdbimage':
				case 'tdxgroupbox':
				case 'tdxpagecontrol':
					ctrl.style.left = x + 'px';
					ctrl.style.top = y + 'px';
					ctrl.style.width = w + 'px';
					ctrl.style.height = h + 'px';
					break;
				case 'tdxcombobox':
					let bw = parseInt(ctrl.nextSibling.style.width);
					ctrl.style.left = x + 'px';
					ctrl.style.top = y + 'px';
					ctrl.style.width = w - bw + 'px';
					ctrl.style.height = h + 'px';
					ctrl.nextSibling.style.left = x + w - bw + 'px';
					ctrl.nextSibling.style.top = y + 'px';
					break;
				case 'tdxlookupcombobox':
				case 'tdxdateedit':
				case 'tdxtimeedit':
				case 'tdxfile':
					ctrl.style.left = x + 'px';
					ctrl.style.top = y + 'px';
					ctrl.style.width = w + 'px';
					ctrl.style.height = h + 'px';
					let bn = ctrl.nextSibling;
					bn.style.left = x + w + 'px';
					bn.style.top = y + 'px';
					bn.style.height = ctrl.style.height;
					bn.style.width = ctrl.style.height;
					break;
				case 'tdxgrid':
				case 'tdxquerygrid':
					if (ctrl.previousSibling.classList.contains('gridcmd')) {
						ctrl.previousSibling.style.left = x + 'px';
						ctrl.previousSibling.style.top = parseInt(ctrl.previousSibling.style.top) + (y - parseInt(ctrl.style.top)) + 'px';
					}
					ctrl.style.left = x + 'px';
					ctrl.style.top = y + 'px';
					ctrl.style.width = w + 'px';
					ctrl.style.height = h + 'px';
					break;
			}
		} else if (jsonObj.prop == 'caption') {
			switch (jsonObj.type) {
				case 'tdxlabel':
					if (ctrl.children.length == 0)
						ctrl.innerHTML = jsonObj.value
					else
						ctrl.children[0].innerHTML = jsonObj.value;
					break;
				case 'tdxbutton':
					ctrl.children[0].innerHTML = jsonObj.value;
					break;
				case 'tdxcheckbox':
					ctrl.nextSibling.nodeValue = jsonObj.value;
					break;
				case 'tdxgroupbox':
					ctrl.querySelector('.groupcap').innerHTML = jsonObj.value;
					break;
				case 'tdxtabsheet':
					let pg = ctrl.closest('div.pages');
					let tabs = pg.children[0];
					let tabsArea = pg.children[1];
					let i = indexNodeOf(tabsArea, ctrl);
					console.log(tabsArea);
					tabs.children[i].innerHTML = jsonObj.value;
					break;
			}
		} else if (jsonObj.prop == 'tabindex') {
			if (jsonObj.type == 'tdxpagecontrol') {
				setActivePage(ctrl, jsonObj.value);
			}
		} else if (jsonObj.prop == 'bitmap') {
			if (jsonObj.type == 'tdximage' || jsonObj.type == 'tdxshape') {
				if (jsonObj.value != '') 
					ctrl.src = jsonObj.value + '#' + Math.floor(Math.random() * 10000)
				else
					ctrl.removeAttribute('src');
			}
		} else if (jsonObj.prop == 'recno') {
			switch (jsonObj.type) {
				case 'tdxform':
				case 'tdxquerygrid':
					let rows = ctrl.getElementsByTagName('TR');
					let i = 1;
					for (row of rows) {
						if (row.getAttribute('class') == 'sel') {
							if (row.dataset.oldClass) {
								row.className = row.dataset.oldClass;
								delete row.dataset.oldClass;
							}
							else
								row.removeAttribute('class');
							break;
						}
						i++;
					}
					let recNo = jsonObj.value - parseInt(ctrl.children[0].dataset.offset);
					if (rows.length > 1 && rows[1].className == 'gridbns-up') recNo++
					let selRow = rows[recNo];
					if (selRow) {
						if (selRow.className) 
							selRow.dataset.oldClass = selRow.getAttribute('class');
						selRow.setAttribute('class', 'sel');
					}
					break;
			}
		}
	}
	for (jsonObj of jsonRoot.tables) {
		let tbl = document.getElementById(jsonObj.table);
		if (tbl) {
			tbl.removeChild(tbl.firstChild);
			tbl.insertAdjacentHTML('afterbegin', jsonObj.html);
		}
	}
	for (jsonObj of jsonRoot.queries) {
		let qry = document.getElementById(jsonObj.query);
		if (qry) {
			qry.removeChild(qry.firstChild);
			qry.insertAdjacentHTML('afterbegin', jsonObj.html);
			let r = qry.querySelector('tr.sel');
			if (r) qry.scrollTop = r.offsetTop - qry.offsetHeight / 2;
		}
	}
	for (jsonObj of jsonRoot.pivots) {
		let pivot = document.getElementById('pgrid' + jsonObj.pivot);
		if (pivot) {
			pivot.removeChild(pivot.firstChild);
			pivot.insertAdjacentHTML('afterbegin', jsonObj.html);
		}
	}	
	let errsEl = document.getElementById('errs');
	let html = '';
	for (jsonObj of jsonRoot.errors) {
		html = html + jsonObj.msg + '<br>';
		if (jsonObj.id > 0) {
			let e = document.getElementById('f' + jsonObj.id);
			if (e) e.classList.add('err');
		}
	}
	
	errsEl.innerHTML = errsEl.innerHTML + html;
	if (html != '') errsEl.style.display = 'block';
	
	if (jsonRoot.docUrl) {
		let a = document.createElement('a');
		a.href = jsonRoot.docUrl;
		a.download = '';
		a.click();
		
		if (jsonRoot.printErrors) alert(jsonRoot.printErrors);
	} else if (jsonRoot.printError) {
		alert(jsonRoot.printError);
	}
	
	if (jsonRoot.debug !== undefined) {
		let dbg = document.getElementById('debug-body');
		if (dbg) {
			dbg.innerHTML += jsonRoot.debug;
			document.getElementById('debug-block').style.display = '';
			dbg.lastChild.scrollIntoView();
		} else {
			let mainBlock = document.getElementById('main-block');
			SendRequest('POST', '?showdbg', '', (Request) => {
				if (Request.status != rcAjaxOk) {
					showAjaxError(Request);
				} else {
					mainBlock.insertAdjacentHTML('beforeend', Request.responseText);
					dbg = document.getElementById('debug-body');
					if (dbg) dbg.lastChild.scrollIntoView();
				}
			});
		}
	}

	if (jsonRoot.msgInfo) {
		let msgBox = new MessageBox(jsonRoot.msgInfo);
		msgBox.onButtonClick = function(id) {
			SendRequest('POST', getCurrentUrl() + '&msgbnclick', 'bn=' + id + '&fresh=' + getFreshValue(), (Request) => {
				msgBox = null;
				if (Request.status != rcAjaxOk) {
					showAjaxError(Request);
				} else {
					processJson(JSON.parse(Request.responseText));
				}
			});
		}
	}
	
}

function tableAdd(id) {
	// Как в queryAdd
	if (fieldChanging) {
		setTimeout(() => tableAdd(qId), 200);
		return;
	}

	SendRequest('POST', getCurrentUrl() + '&tableadd', 'id=' + id + '&fresh=' + getFreshValue(), (Request) => {
		if (Request.status == rcAjaxOk) location.href = Request.responseText
		else showAjaxError(Request);
	});
}

function tableClick(ev, isQuery) {
	let el = ev.target;
	let editClicked = false;
	let delClicked = false;
	if (el.tagName == 'IMG') {
		if (el.className == 'edit') editClicked = true
		else if (el.className == 'del') {
			if (!confirm(rsDeleteRecMsg)) return;
			delClicked = true;
		}
		el = el.parentElement;
	}
	if (el.tagName == 'TD') {
		if (el.parentElement.className == 'gridbns-up' || el.parentElement.className == 'gridbns-down') {
			ev.preventDefault();
			return;
		}
		
		let rows = ev.currentTarget.getElementsByTagName('TR');
		let i = 1;
		for (row of rows) {
			if (row.getAttribute('class') == 'sel') {
				if (row.dataset.oldClass) {
					row.className = row.dataset.oldClass;
					delete row.dataset.oldClass;
				}
				else
					row.removeAttribute('class');
				break;
			}
			i++;
		}
		let selRow = el.parentElement;
		if (selRow.className) 
			selRow.dataset.oldClass = selRow.getAttribute('class');
		selRow.setAttribute('class', 'sel');
		for (i = 0; i < rows.length; i++) {
			if (rows[i] == selRow) break;
		}

		let gridId = ev.currentTarget.parentElement.id.substring(1);
		if (rows[1].className == 'gridbns-up') i--;
		let recNo = parseInt(ev.currentTarget.dataset.offset) + i;
		
		if (editClicked) {
			SendRequest('POST', getCurrentUrl() + (isQuery ? '&queryedit' : '&tableedit'), 'id=' + gridId + '&row=' + recNo + '&fresh=' + getFreshValue(), (Request) => {
				if (Request.status == rcAjaxOk) goUrl(Request.responseText, false)
				else showAjaxError(Request);
			})
		} else if (delClicked) {
			SendRequest('POST', getCurrentUrl() + (isQuery ? '&querydel' : '&tabledel'), 'id=' + gridId + '&row=' + recNo + '&fresh=' + getFreshValue(), (Request) => {
				if (Request.status == rcAjaxOk) {
					processJson(JSON.parse(Request.responseText));
					getDataStateDS().modified = 1;
				} else showAjaxError(Request);
			})
		} else {
			SendRequest('POST', getCurrentUrl() + (isQuery ? '&queryscroll' : '&tablescroll'), 'id=' + gridId + '&row=' + recNo + '&fresh=' + getFreshValue(), (Request) => {
				if (Request.status == rcAjaxOk) processJson(JSON.parse(Request.responseText))
				else showAjaxError(Request);
			})
		}
	}
}

function tableFetch(id, isQuery, skip, dir) {
	let tblDiv = document.getElementById( (isQuery ? 'q' : 't') + id );
	
	let tbl = tblDiv.children[0];
	let rows = tbl.getElementsByTagName('TR');
	let rowMoreHeight = 0;
	if (dir == 1) {
		rowMoreHeight = rows[1].offsetHeight;
		rows[1].remove();
	} else {
		rows[rows.length - 1].remove();
	}
	//let rowCount = rows.length - 1;
	SendRequest('POST', getCurrentUrl() + (isQuery ? '&queryfetch' : '&tablefetch'), 'id=' + id + '&skip=' + skip + '&dir=' + dir + '&fresh=' + getFreshValue(), (Request) => {
			if (Request.status == rcAjaxOk) {
				let html = Request.responseText;
				if (dir == 1) {
					let r = rows[1];
					r.insertAdjacentHTML('beforebegin', html);
					tblDiv.scrollTop = r.offsetTop - rowMoreHeight;
					let tmp = parseInt(tbl.dataset.offset) - 100;
					if (tmp < 0) tmp = 0;
					tbl.dataset.offset = tmp;
				} else {
					let lastRow = rows[rows.length - 1];
					lastRow.insertAdjacentHTML('afterend', html);
				}
			}
			else showAjaxError(Request);
		})
}

function tableScroll() {
	let tblDiv = event.currentTarget;
	if (tblDiv.scrollTop == 0) {
		let bn = tblDiv.querySelector('button.gridbns-more-up');
		if (bn) bn.click();
	} else if (tblDiv.scrollTop == tblDiv.scrollHeight - tblDiv.clientHeight) {
		let bn = tblDiv.querySelector('button.gridbns-more-down');
		if (bn) bn.click();
	}
}

/*function findElementByAttrValue(tags, attrName, attrValue) {
	for (let i = 0; i < tags.length; i++) {
		if (tags[i].getAttribute(attrName) == attrValue) return tags[i];
	}
	return false;
}*/

function fieldChange(el, sync = false) {

    let Handler = function(Request)
    {
		fieldChanging = false;
		if (Request.status == rcAjaxOk) {
			processJson(JSON.parse(Request.responseText));
		} else {
			showAjaxError(Request);
		}
    }
	let v = el.value;
	if (el.type == 'checkbox') {
		if (v == '0' || v == '') v = '1'
		else v = '0';
	}
	fieldChanging = true;
	getDataStateDS().modified = 1;
    SendRequest('POST', getCurrentUrl() + '&fieldchange', el.name + "=" + encodeURIComponent(v) + '&fresh=' + getFreshValue(), Handler, sync, false);
}

function fieldChangeById(cid) {
	fieldChange(document.getElementById('f' + cid));
}

function templateClick(el) {
	SendRequest('POST', getCurrentUrl() + '&print=' + el.dataset.index, 'fresh=' + getFreshValue(), (Request) => {
		if (Request.status != rcAjaxOk) {
			showAjaxError(Request);
			return;
		}
		let jsonObj = JSON.parse(Request.responseText);
		if (jsonObj.file) {
			let a = document.createElement('a');
			a.href = jsonObj.file;
			a.download = '';
			a.click();
			
			if (jsonObj.errors) alert(jsonObj.errors);
		} else if (jsonObj.error) {
			alert(jsonObj.error);
		}
	});
}

function printClick() {
	let el = document.getElementById('templates');
	if (el.style.display == '') el.style.display = 'none'
	else el.style.display = '';
}

function deleteClick() {
	if (confirm(rsDeleteRecMsg)) {
		SendRequest('POST', getCurrentUrl() + '&del', 'fresh=' + getFreshValue(), (Request) => {
			if (Request.status == rcAjaxOk) goBack()
			else showAjaxError(Request);
		});
	}
}

function cancelClick() {
	let ds = getDataStateDS();
	if (ds.confirmCancel == 1 && ds.modified == 1) {
		if (!confirm(rsConfirmCancelMsg)) return;
	}
	SendRequest('POST', getCurrentUrl() + '&cancel', '', (Request) => {
		if (Request.status == rcAjaxOk) goBack()
		else showAjaxError(Request);
	});
}

function okClick() {
	let ds = getDataStateDS();
	if (ds.confirmSave == 1 && ds.modified == 1) {
		if (!confirm(rsConfirmSaveMsg)) return;
	}
	
	// Несмотря на то, что change поля срабатывает раньше onclick кнопки, 
	// почему-то сервер может сначала получить запрос от кнопки, а уже потом от поля. И получается, что связь с формой 
	// разрывается в то время, когда на сервер "летит" изменение поля. Поэтому сделал обходной путь с таймером и 
	// флагом fieldChanging.
	let bn = event.currentTarget;
	if (fieldChanging) {
		setTimeout(() => bn.click(), 200);
		return;
	}
	SendRequest('POST', getCurrentUrl() + '&post',  'fresh=' + getFreshValue(), (Request) => {
		if (Request.status == rcAjaxOk) {
			goBack();
		} else if (Request.status == rcValidateError) {
			processJson(JSON.parse(Request.responseText));
		} else {
			showAjaxError(Request);
		}
	});
}

function formKeyDown(ev) {
	if (ev.ctrlKey && ev.keyCode == 13) { 
		let okBn = document.getElementById('okbn');
		if (!okBn) return;
		
		if (ev.target.hasAttribute('onchange')) {
			fieldChange(ev.target, true);
		}
		okBn.click(); 
	}
}

function headerClick() {
	let el = event.currentTarget;
	let div = el.closest('div');
	let params = 'qid=' + div.id.substr(1) + '&fid=' + el.dataset.fid;
	if (el.dataset.order) {
		params += '&order=' + el.dataset.order;
	}
	if (event.ctrlKey) params += '&add=1';
	SendRequest('POST', getCurrentUrl() + '&sort', params + '&fresh=' + getFreshValue(), (Request) => {
		if (Request.status == rcAjaxOk) {
			processJson(JSON.parse(Request.responseText));
		} else {
			showAjaxError(Request);
		}
	});
}

function doDuplicate(href) {
	// Несмотря на то, что change поля срабатывает раньше onclick кнопки, 
	// почему-то сервер может сначала получить запрос от кнопки, а уже потом от поля. И получается, что связь с формой 
	// разрывается в то время, когда на сервер "летит" изменение поля. Поэтому сделал обходной путь с таймером и 
	// флагом fieldChanging.
	if (fieldChanging) {
		setTimeout(() => doDuplicate(href), 200);
		return;
	}
	SendRequest('POST', href, 'fresh=' + getFreshValue(), (Request) => {
		if (Request.status == rcAjaxOk) {
			let res = Request.responseText.trim();
			location.replace(res);
		} else if (Request.status == rcValidateError) {
			processJson(JSON.parse(Request.responseText)); 
		} else {
			showAjaxError(Request);
		}
	});
}

function showDuplicateMenu(bn) {
	if (dupMnu) {
		dupMnu.hide();
		return;
	}
	dupMnu = new DuplicateMenu(bn);
	dupMnu.show();
	dupMnu.onHide = function(that) {
		if (that == dupMnu) dupMnu = null;
	}
	dupMnu.onClick = function(href) {
		doDuplicate(href);
	}
}

function duplicateClick() {
	let bn = event.currentTarget;
	if (bn.hasAttribute('data-haschilds')) {
		showDuplicateMenu(bn);
	} else {
		doDuplicate(getCurrentUrl() + '&dup')
		//window.location = getCurrentUrl() + '&dup';
	}
}

function queryAdd(qId) {
	// Несмотря на то, что change поля срабатывает раньше onclick кнопки, 
	// почему-то сервер может сначала получить запрос от кнопки, а уже потом от поля. И получается, что связь с формой 
	// разрывается в то время, когда на сервер "летит" изменение поля. Поэтому сделал обходной путь с таймером и 
	// флагом fieldChanging.
	if (fieldChanging) {
		setTimeout(() => queryAdd(qId), 200);
		return;
	}

	let ds = getDataStateDS();
	if (ds.parentState == 'insert') {
		alert(rsFirstSaveParentRecord);
		return;
	} else if (ds.state == 'insert' && !confirm(rsFirstSaveRecord)) return;
	
	SendRequest('POST', getCurrentUrl() + '&queryadd', 'id=' + qId + '&fresh=' + getFreshValue(), (Request) => {
		if (Request.status == rcAjaxOk) goUrl(Request.responseText, false)
		else if (Request.status == rcValidateError) processJson(JSON.parse(Request.responseText))
		else showAjaxError(Request);
	});
}

function bnClick(bnName) {
	SendRequest('POST', getCurrentUrl() + '&bnclick', 'bn=' + bnName + '&fresh=' + getFreshValue(), (Request) => {
		if (Request.status == rcAjaxOk) {
			processJson(JSON.parse(Request.responseText));
		} else {
			showAjaxError(Request);
		}
	});
}

function bodyLoad() {
	let checks = document.querySelectorAll('input[type=checkbox]');
	for (let chk of checks) {
		if (chk.value == '') chk.indeterminate = true;
	}
	
	let rows = document.querySelectorAll('div.grid tr.sel');
	for (let r of rows) {
		let div = r.parentElement.parentElement.parentElement;
		div.scrollTop = r.offsetTop - div.offsetHeight / 2;
	}
	
	let item = document.querySelector('div.sidebar a.sel');
	if (item) item.scrollIntoView({block: "center"});
	
	let dbgBody = document.getElementById('debug-body');
	if (dbgBody && dbgBody.hasChildNodes()) dbgBody.lastChild.scrollIntoView();
	
	let msgInfo = document.getElementById('$msginfo');
	if (msgInfo) {
		let msgBox = new MessageBox(JSON.parse(msgInfo.innerHTML));
		msgBox.onButtonClick = function(id) {
			SendRequest('POST', getCurrentUrl() + '&msgbnclick', 'bn=' + id + '&fresh=' + getFreshValue(), (Request) => {
				msgBox = null;
				if (Request.status != rcAjaxOk) {
					showAjaxError(Request);
				} else {
					processJson(JSON.parse(Request.responseText));
				}
			});
		}
	}
	
	let selTabs = document.querySelectorAll('.pages span.sel');
	for (let tab of selTabs) {
		let tabs = tab.parentElement;
		if (tab.offsetLeft + tab.offsetWidth > tabs.scrollLeft + tabs.offsetWidth) {
			tab.scrollIntoView({inline: "center"});
		}
	}
}