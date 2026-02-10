const
	rcOk = 200;
	rcAjaxOk = 222;
	rcAjaxError = 223;
	rcValidateError = 224;
	
	gtoDefault = 0;
	gtoReplaceUrl = 1;
	gtoNewTab = 2;

/////////////////////////////////////////////////////////////////////////////////////////
// Ajax-запросы
/////////////////////////////////////////////////////////////////////////////////////////

function CreateRequest()
{
    let Request = false;

    if (window.XMLHttpRequest)
    {
        //Gecko-совместимые браузеры, Safari, Konqueror
        Request = new XMLHttpRequest();
    }
    /*else if (window.ActiveXObject)
    {
        //Internet explorer
        try
        {
             Request = new ActiveXObject("Microsoft.XMLHTTP");
        }    
        catch (CatchException)
        {
             Request = new ActiveXObject("Msxml2.XMLHTTP");
        }
    }*/
 
    if (!Request)
    {
        alert("Невозможно создать XMLHttpRequest");
    }
    
    return Request;
} 

/*
Функция посылки запроса к файлу на сервере
r_method  - тип запроса: GET или POST
r_path    - путь к файлу
r_args    - аргументы вида a=1&b=2&c=3...
r_handler - функция-обработчик ответа от сервера
*/
function SendRequest(r_method, r_path, r_args, r_handler, sync = false, curtainShow = true)
{
    //Создаём запрос
    let Request = CreateRequest();
    
    //Проверяем существование запроса еще раз
    if (!Request)
    {
        return;
    }
    
    //Назначаем пользовательский обработчик
    Request.onreadystatechange = function()
    {
        //Если обмен данными завершен
        if (Request.readyState == 4)
        {
			if (curtainShow) hideCurtain();
            //Передаем управление обработчику пользователя
            if (r_handler) r_handler(Request);
        }
    }
	
	Request.onerror = function()
	{
		if (curtainShow) hideCurtain();
	}
    
	if (curtainShow) showCurtain();
	
    //Проверяем, если требуется сделать GET-запрос
    if (r_method.toLowerCase() == "get" && r_args.length > 0)
    r_path += r_args;
    
    //Инициализируем соединение
    Request.open(r_method, r_path, !sync);
    
    if (r_method.toLowerCase() == "post")
    {
        //Если это POST-запрос
        
        //Устанавливаем заголовок
        Request.setRequestHeader("Content-Type","application/x-www-form-urlencoded; charset=utf-8");
        //Посылаем запрос
        Request.send(r_args);
    } else if (r_method.toLowerCase() == 'put') 
	{
        Request.send(r_args);
	}
    else
    {
        //Если это GET-запрос
        
        //Посылаем нуль-запрос
        Request.send(null);
    }
} 

/////////////////////////////////////////////////////////////////////////////////////////
// Разные функции
/////////////////////////////////////////////////////////////////////////////////////////

function isContained(parentEl, el) {
	if (el) {
		if (el == parentEl) return true
		else return isContained(parentEl, el.parentNode);
	} else {
		return false;
	}
}

function getCurrentUrl() {
	return location.search;//document.URL.substring(document.URL.indexOf('?'));
}

function gotoUrl(url) {
	showCurtain();
	location.href = url;
}

function reloadPage() {
	/*let waitEl = new Curtain();
	waitEl.show();*/
	showCurtain();
	location.reload();
}

function parseDate(date) {
	if (date == '') return false;
	let fmt = formatSettings.shortDateFormat;
	let sep = formatSettings.dateSeparator;
	let fmtItems = fmt.split(sep);
	let p = 0;
	let d, m, y;
	let part = '';
	
	let lastCh = date.at(-1);
	if (lastCh >= '0' && lastCh <= '9')	date = date + sep;
	
	for (let i = 0; i < date.length; i++) {
		let ch = date[i];
		if (ch >= '0' & ch <= '9') {
			part = part + ch;
		} else if ((ch == sep || ch == ' ') && part != '') {
			if (fmtItems[p][0] == 'd') d = parseInt(part)
			else if (fmtItems[p][0] == 'm') m = parseInt(part)
			else y = parseInt(part);
			part = '';
			p++;
		} else {
			return false;
		}
	}
	
	if (d == undefined || m == undefined || y == undefined) {
		let curDate = new Date();
		if (d == undefined) d = 1;
		if (m == undefined) m = curDate.getMonth() + 1;
		if (y == undefined) y = curDate.getFullYear();
	}
	if (y < 100) {
		let dt = new Date();
		if (Math.abs((2000 + y) - dt.getFullYear()) >= 50) y = 1900 + y
		else y = 2000 + y;
	}
	let dt = new Date(y, m - 1, d);
	if (dt.getDate() != d) dt = false;
	return dt;
}

function formatDate(date) {
	let fmt = formatSettings.shortDateFormat;
	let sep = formatSettings.dateSeparator;
	let fmtItems = fmt.split(sep);
	let dateStr = '';
	
	for (let i = 0; i < fmtItems.length; i++) {
		if (fmtItems[i][0] == 'd') {
			let d = date.getDate();
			if (fmtItems[i].length > 1 && d < 10) d = '0' + d;
			dateStr = dateStr + d + sep;
		} else if (fmtItems[i][0] == 'm') {
			let m = date.getMonth() + 1;
			if (fmtItems[i].length > 1 && m < 10) m = '0' + m;
			dateStr = dateStr + m + sep;
		} else if (fmtItems[i][0] == 'y') {
			let y = date.getFullYear();
			if (fmtItems[i].length < 4) y = y.toString().slice(2);
			dateStr = dateStr + y + sep;
		}
	}
	return dateStr.slice(0, -1);
}

function parseTime(time) {
	if (time == '') return false;
	let fmt = formatSettings.longTimeFormat;
	let sep = formatSettings.timeSeparator;
	let fmtItems = fmt.split(sep);
	let p = 0;
	
	let lastCh = time.at(-1);
	if (lastCh >= '0' && lastCh <= '9')	time = time + sep;
	
	let part = '', h, m;
	for (let i = 0; i < time.length; i++) {
		let ch = time[i];
		if (ch >= '0' & ch <= '9') {
			part += ch;
		} else if ((ch == sep || ch == ' ') && part != '') {
			if (h == undefined) h = parseInt(part)
			else if (m == undefined) {
				m = parseInt(part);
				break;
			}
			part = '';
			p++;
		} else return false;
	}
	
	if (h > 23 || m > 59) return false;
	
	let tm = new Date();
	tm.setHours(h);
	if (m != undefined) tm.setMinutes(m)
	else tm.setMinutes(0);
	return tm;
}

function formatTime(time) {
	let fmt = formatSettings.longTimeFormat;
	let sep = formatSettings.timeSeparator;
	let fmtItems = fmt.split(sep);
	let timeStr = '';
	
	for (let i = 0; i < fmtItems.length; i++) {
		if (fmtItems[i][0] == 'h') {
			let h = time.getHours();
			if (fmtItems[i].length > 1 && h < 10) h = '0' + h;
			timeStr = timeStr + h + sep;
		} else if (fmtItems[i][0] == 'm') {
			let m = time.getMinutes();
			if (fmtItems[i].length > 1 && m < 10) m = '0' + m;
			timeStr = timeStr + m + sep;
		} else if (fmtItems[i][0] == 's') {
			let s = time.getSeconds();
			if (fmtItems[i].length > 1) s = '0' + s;
			timeStr = timeStr + s + sep;
		}
	}
	return timeStr.slice(0, -1);
}

function showAjaxError(Request) {
	if (Request.status == rcAjaxError) {
		let jsonObj = JSON.parse(Request.responseText);
		alert(jsonObj.error);
		if (jsonObj.code == 9) reloadPage(); // location.reload();
	} else if (Request.status == 401) {
		alert(rsSessionExpired);
		reloadPage();
		//location.reload();
	} else {
		alert(rsSomethingWrong + "\n" + Request.responseText);
	}
}

function indexNodeOf(parentEl, child) {
	for (let i = 0; i < parentEl.children.length; i++) {
		if (parentEl.children[i] == child) return i;
	}
	return -1;
}

/////////////////////////////////////////////////////////////////////////////////////////
// Объект и список с источником
/////////////////////////////////////////////////////////////////////////////////////////

function listScroll() {
	let div = event.currentTarget;
	if (div.scrollTop + div.clientHeight == div.scrollHeight) {
		let bn = div.querySelector('input[type=button]');
		if (bn) bn.click();
	}
}

class ListCbx {
	
	constructor (cbx, cid) {
		this.cbx = cbx;
		this.cid = cid;
		this.bn = this.cbx.nextSibling;
		this.key = this.cbx.previousSibling;
		this.isLCbx = (this.key && this.key.type == 'hidden');
		this.isMobile = window.matchMedia('(pointer:coarse)').matches;
		this.isFilter = !this.cbx.hasAttribute('id');
		this.showAsTree = this.cbx.hasAttribute('show-tree');
		this.row = 0;
		this.onHide = null;
		this.onSelect = null;
	}
	
	getParams(key = false, skip = false) {
		let params = 'id=' + this.cid;
		let len = this.tbl.rows.length;
		if (skip && len > 1) params += '&skip=' + (len - 2);
		if (this.fltInp.value != '') params += '&frags=' + encodeURIComponent(this.fltInp.value);
		return params;
	}
	
	getMoreList = (ev) => {
		SendRequest('POST', getCurrentUrl() + '&getlist', this.getParams(false, true), (Request) => {
			if (Request.status != rcAjaxOk) {
				showAjaxError(Request);
				this.hide();
				return;
			}
			if (!this.lst.parentNode) return;

			let lastRow = this.tbl.rows[this.tbl.rows.length - 1];
			let startRow = this.tbl.rows.length - 1;

			let lastRowSel = (lastRow.className == 'sel');
			let prevRow = lastRow.previousSibling;
			lastRow.insertAdjacentHTML('beforebegin', Request.responseText);
			lastRow.remove();
			if (lastRowSel) {
				prevRow.className = 'sel';
				this.row--;
			}
			this.setMoreButtonClickHandler();
			this.selectValue(startRow, false);
			if (!this.isMobile) this.fltInp.focus();
		});
	}
	
	cbxInputHandler = (ev) => {
		this.fltInp.removeEventListener('input', this.cbxInputHandler);
		let oldValue = this.fltInp.value;

		SendRequest('POST', getCurrentUrl() + '&getlist', this.getParams(), (Request) => {
			if (Request.status != rcAjaxOk) {
				showAjaxError(Request);
				this.hide();
				return;
			}
			if (!this.lst.parentNode) return;
			
			this.fltInp.addEventListener('input', this.cbxInputHandler);
			if (this.fltInp.value == oldValue) {
				this.tbl.innerHTML = Request.responseText;
				if (!this.isMobile) {
					if (this.tbl.offsetHeight < 300 - this.flt.offsetHeight) {
						//this.lst.style.height = (this.tbl.offsetHeight + this.flt.offsetHeight + 2) + 'px';
						this.lst.style.height = (this.tbl.offsetHeight + this.flt.offsetHeight + this.btns.offsetHeight) + 'px';
					} else {
						this.lst.style.height = 300 + 'px';
					}
					this.tblPar.style.height = this.lst.clientHeight - this.flt.offsetHeight - this.btns.offsetHeight + 'px';
				}
				this.row = 0;
				this.tblPar.scrollTop = 0;
				if (this.tbl.rows.length > 1) {
					this.tbl.rows[1].className = 'sel';
					this.row = 1;
				}
				this.setMoreButtonClickHandler();
			} else {
				let e = new Event('input');
				this.fltInp.dispatchEvent(e);
			}
		}, false, false);
	}

	cbxKeyDownHandler = (ev) => {
		let rows = this.tbl.rows;
		if (ev.code == 'ArrowDown' || ev.code == 'ArrowUp') {
			let r = this.row;
			if (r > 0) rows[r].className = '';
			if (ev.code == 'ArrowDown') {
				if (r < rows.length - 1) r++;
			} else if (r > 1) r--;
			this.row = r;
			let row = rows[r];
			let rTop = this.tbl.offsetTop + row.offsetTop;
			let rBottom = rTop + row.offsetHeight;
			let sTop = this.tblPar.scrollTop + this.tbl.offsetTop;
			let sBottom = this.tblPar.scrollTop + this.tblPar.clientHeight;
			if (r > 0) rows[r].className = 'sel';
			if (rTop >= sBottom) row.scrollIntoView({block: "center"})
			else if (rBottom > sBottom) row.scrollIntoView({block: "end"})
			else if (rBottom - 10 <= sTop) row.scrollIntoView({block: "center"})
			else if (rTop < sTop && rBottom > sTop) this.tblPar.scrollTop = rTop - this.tbl.offsetTop;
			// Предотвращаем движение курсора в конец и начало текста в поле ввода
			ev.preventDefault();
			ev.stopPropagation();
		} else if (ev.code == 'PageDown') {
			let r = this.row;
			if (r > 0) rows[r].className = '';
			let yOffset = rows[r].offsetTop - this.tblPar.scrollTop;
			let yBottom = this.tblPar.scrollTop + this.tblPar.clientHeight;
			for (; r < rows.length - 1 && rows[r+1].offsetTop < yBottom + yOffset; r++) {}
			if (r > 0) rows[r].className = 'sel';
			this.tblPar.scrollTop = yBottom - (yOffset - (rows[r].offsetTop - yBottom));
			this.row = r;
		} else if (ev.code == 'PageUp') {
			let r = this.row;
			if (r > 0) rows[r].className = '';
			let yOffset = rows[r].offsetTop - this.tblPar.scrollTop;
			let yTop = this.tblPar.scrollTop - this.tblPar.clientHeight;
			for (; r > 1 && rows[r-1].offsetTop > yTop + yOffset; r--) {}
			if (r > 0) rows[r].className = 'sel';
			this.tblPar.scrollTop = yTop - (yOffset - (rows[r].offsetTop - yTop));
			this.row = r;
		} else if (ev.code == 'Enter') {
			let row = this.tbl.querySelector('.sel');
			if (row) {
				let bn = row.querySelector('input');
				if (bn) {
					bn.click();
					ev.preventDefault();
					ev.stopPropagation();
				} else {
					this.setValue(row.firstChild);
					if (this.onSelect) this.onSelect(this);
					this.hide();
					ev.preventDefault();
					ev.stopPropagation();
				}
			}
		} else if (ev.code == 'Escape' || ev.code == 'Tab') {
			this.hide();
		}
	}
	
	anyClickHandler = (ev) => {
		if (isContained(this.lst, ev.target)) {
			if (ev.target.tagName == 'TD') {
				if (!ev.target.parentNode.querySelector('input')) {
					this.setValue(ev.target.parentElement.children[0]);
					if (this.onSelect) this.onSelect(this);
					this.hide();
				}
			}
		} else {
			this.hide();
		}
	}
	
	setMoreButtonClickHandler() {
		let tags = this.tbl.getElementsByTagName('input');
		if (tags.length == 1) tags[0].addEventListener('click', this.getMoreList);
	}
	
	hide () {
		if (this.lst.parentNode) this.lst.remove();
		window.removeEventListener('click', this.anyClickHandler);
		if (document.activeElement == document.body) this.cbx.focus();
		if (this.onHide) this.onHide(this);
	}
	
	getExtraWidth() {
		if (this.cbx.hasAttribute('extra-width')) return parseInt(this.cbx.getAttribute('extra-width'))
		else return 0;
	}
	
	setValue (td) {
		let indent = ' '.repeat(parseInt(td.style.paddingLeft) / 10);
		if (td.hasAttribute('empty')) this.cbx.value = indent
		else if (this.isLCbx && this.showAsTree) { }		// Чтобы текст в поле не мельтешил, все равно сервер вернет правильное значение
		else this.cbx.value = indent + td.textContent;
		if (this.isLCbx) {
			this.key.value = td.parentNode.getAttribute('key');
			let e = new Event('change');
			this.key.dispatchEvent(e);
		} else {
			let e = new Event('change');
			this.cbx.dispatchEvent(e);
		}
	}
	
	selectValue(startRow, scrollVw) {
		if (this.row > 0) return;
		
		let rows = this.tbl.rows;
		if (this.isLCbx) {
			let key = this.key.value;
			if (key == '') return;
			for (let i = startRow; i < rows.length; i++) {
				if (key == rows[i].getAttribute('key')) {
					rows[i].className = 'sel';
					this.row = i;
					this.itemSelected = true;
					if (scrollVw) {
						rows[i].scrollIntoView({block: "center"});
					}
					break;
				}
			}
		} else {
			let value = this.cbx.value;
			if (value == '') return;
			for (let i = startRow; i < rows.length; i++) {
				if (value.localeCompare(rows[i].textContent, undefined, { sensitivity: 'accent' }) == 0) {
					rows[i].className = 'sel';
					this.row = i;
					if (scrollVw) {
						rows[i].scrollIntoView({block: "center"});
					}
					break;
				}
			}
		}
	}
	
	listBtnClickHandler = (ev) => {
		let el = ev.currentTarget;
		if (el.tagName != 'BUTTON') return;
		if (el == el.parentElement.children[0]) {
			SendRequest('POST', getCurrentUrl() + '&objadd', 'id=' + this.cid, (Request) => {
				if (Request.status == rcAjaxOk) gotoUrl(Request.responseText)
				else showAjaxError(Request);
			})
		} else if (el == el.parentElement.children[1] && this.key.value) {
			SendRequest('POST', getCurrentUrl() + '&objedit', 'id=' + this.cid + '&rec=' + this.key.value, (Request) => {
				if (Request.status == rcAjaxOk) gotoUrl(Request.responseText)
				else showAjaxError(Request);
			})
		} else if (el == el.parentElement.children[2]) {
			gotoUrl('?fm=' + this.cbx.dataset.fm);
		} else if (el == el.parentElement.children[3]) {
			this.hide();
		}
	}
	
	show() {
		this.lst = document.createElement('div');
		this.lst.className = 'listcbx';
		this.lst.style.position = 'absolute';
		this.lst.style.zIndex = 10000;
		this.lst.style.background = 'white';
		this.lst.innerHTML = '<div style="padding: 4px; background: #eee;"><input type=text style="width: 100%; padding: 4px;" ></div>' +
			'<div style="width: 100%; overflow-x: hidden; overflow-y: auto;" onscroll="listScroll()"><table class=list></table></div>' +
			'<div class=listbtns><button type=button><img src="/img/add.svg"></button><button type=button><img src="/img/editobj.svg"></button>' +
			'<button type=button><img src="/img/table.svg"></button><button type=button><img src="/img/cancel.svg"></button></div>';
		this.lst.style.visibility = 'hidden';
		this.flt = this.lst.firstChild;
		this.fltInp = this.flt.firstChild;
		this.tblPar = this.lst.firstChild.nextSibling;
		this.tbl = this.tblPar.firstChild;
		this.btns = this.tblPar.nextSibling;
		
		if (this.isMobile) {
			this.lst.style.position = 'fixed';
			this.lst.style.left = '20px';
			this.lst.style.top = '20px';
			this.lst.style.width = 'calc(100% - 40px)';
			this.lst.style.height = 'calc(100% - 40px)';
			this.lst.style.border = '1px solid black';
		} else {
			let coords = this.cbx.getBoundingClientRect();
			this.lst.style.left = coords.left + window.pageXOffset + 'px';  //rect.left + 'px';
			this.lst.style.top = coords.bottom + window.pageYOffset + 'px';  //rect.bottom + 'px';
			let w = this.cbx.offsetWidth + this.bn.offsetWidth + this.getExtraWidth();
			if (w < 160 && this.isLCbx) w = 160;
			this.lst.style.width = w + 'px';
			this.lst.style.border = '1px solid black';
		}

		if (this.isFilter || !this.isLCbx || !this.cbx.dataset.access) {
			if (this.isMobile) {
				this.btns.children[0].style.display = 'none';
				this.btns.children[1].style.display = 'none';
				this.btns.children[2].style.display = 'none';
			} else {
				this.btns.style.display = 'none';
			}
		}
		if (this.cbx.dataset.access) {
			if (this.cbx.dataset.access < 3) this.btns.children[0].style.display = 'none'; 
			if (this.cbx.dataset.access < 2) this.btns.children[1].firstChild.src = '/img/view.svg'; 
			if (!this.isMobile) this.btns.children[3].style.display = 'none';
		}
		
		for (let bn of this.btns.children) 
			bn.addEventListener('click', this.listBtnClickHandler);
		
		document.body.appendChild(this.lst);
		
		SendRequest('POST', getCurrentUrl() + '&getlist', this.getParams(true), (Request) => {
			if (Request.status != rcAjaxOk) {
				showAjaxError(Request);
				this.hide();
				return;
			}
			if (document.activeElement != this.cbx && document.activeElement != this.bn) {
				// Здесь вызываем hide, потому что anyClickHandler еще не назначен.
				this.hide();
				return;
			}
			
			this.tbl.innerHTML = Request.responseText;
			if (this.isMobile) {
				this.tblPar.style.height = 'calc(100% - ' + (this.flt.offsetHeight + this.btns.offsetHeight) + 'px)';
			} else {
				if (this.tbl.offsetHeight < 300 - this.flt.offsetHeight) {
					this.lst.style.height = (this.tbl.offsetHeight + this.flt.offsetHeight + this.btns.offsetHeight) + 'px';
				} else {
					this.lst.style.height = '300px';
				}
				this.tblPar.style.height = this.lst.clientHeight - this.flt.offsetHeight - this.btns.offsetHeight + 'px';
			}			
			this.selectValue(0, true);
			this.flt.style.visibility = 'visible';
			this.lst.style.visibility = 'visible';
			this.setMoreButtonClickHandler();
			if (!this.isMobile) this.fltInp.focus();
			
			this.fltInp.addEventListener('input', this.cbxInputHandler);
			this.fltInp.addEventListener('keydown', this.cbxKeyDownHandler);
			window.addEventListener('click', this.anyClickHandler);
		});
	}
	
}

/////////////////////////////////////////////////////////////////////////////////////////
// Фиксированный список
/////////////////////////////////////////////////////////////////////////////////////////

class FixedListCbx {

	constructor (cbx) {
		this.cbx = cbx;
		this.bn = this.cbx.nextSibling;
		this.lst = document.getElementById(cbx.id + 'l');
		if (this.lst.parentElement != document.body) {
			this.cbx.parentElement.removeChild(this.lst);
			document.body.appendChild(this.lst);
		}
		this.tbl = this.lst.firstChild;
		this.isMobile = window.matchMedia('(pointer:coarse)').matches;
		this.row = -1;
		this.stopClick = false;
		this.onHide = null;
		this.onSelect = null;
	}
	
	cbxKeyDownHandler = (ev) => {
		let rows = this.tbl.rows;
		if (ev.code == 'ArrowDown' || ev.code == 'ArrowUp') {
			let r = this.row;
			if (r >= 0) rows[r].className = '';
			if (ev.code == 'ArrowDown') {
				if (r < rows.length - 1) r++;
			} else if (r > 0) r--;
			this.row = r;
			let row = rows[r];
			let rTop = row.offsetTop;
			let rBottom = rTop + row.offsetHeight;
			let sTop = this.lst.scrollTop;
			let sBottom = this.lst.scrollTop + this.lst.clientHeight;
			if (r >= 0) rows[r].className = 'sel';
			if (rTop >= sBottom) row.scrollIntoView({block: "center"})
			else if (rBottom > sBottom) row.scrollIntoView({block: "end"})
			else if (rBottom - 10 <= sTop) row.scrollIntoView({block: "center"})
			else if (rTop < sTop && rBottom > sTop) this.lst.scrollTop = rTop;
			// Предотвращаем движение курсора в конец и начало текста в поле ввода
			ev.preventDefault();
			ev.stopPropagation();
		} else if (ev.code == 'PageDown') {
			let r = this.row;
			if (r >= 0) rows[r].className = ''
			else r = 0;
			let yOffset = rows[r].offsetTop - this.lst.scrollTop;
			let yBottom = this.lst.scrollTop + this.lst.clientHeight;
			for (; r < rows.length - 1 && rows[r+1].offsetTop < yBottom + yOffset; r++) {}
			if (r >= 0) rows[r].className = 'sel';
			this.lst.scrollTop = yBottom - (yOffset - (rows[r].offsetTop - yBottom));
			this.row = r;
		} else if (ev.code == 'PageUp') {
			let r = this.row;
			if (r >= 0) rows[r].className = '';
			let yOffset = rows[r].offsetTop - this.lst.scrollTop;
			let yTop = this.lst.scrollTop - this.lst.clientHeight;
			for (; r > 0 && rows[r-1].offsetTop > yTop + yOffset; r--) {}
			if (r >= 0) rows[r].className = 'sel';
			this.lst.scrollTop = yTop - (yOffset - (rows[r].offsetTop - yTop));
			this.row = r;
		} else if (ev.code == 'Enter') {
			let row = this.tbl.querySelector('.sel');
			if (row) {
				this.setValue(row.firstChild);
				if (this.onSelect) this.onSelect(this);
				this.hide();
				ev.preventDefault();
				ev.stopPropagation();
			}
		} else if (ev.code == 'Escape' || ev.code == 'Tab') {
			this.hide();
		}
	}

	anyClickHandler = (ev) => {
		if (isContained(this.lst, ev.target)) {
			if (ev.target.tagName == 'TD') {
				this.setValue(ev.target);
				if (this.onSelect) this.onSelect(this);
				this.hide();
			}
		} else {
			if (this.stopClick)	this.stopClick = false
			else this.hide();
		}
	}

	setValue(td) {
		if (td.hasAttribute('empty')) this.cbx.value = ''
		else this.cbx.value = td.textContent;
		let e = new Event('change');
		this.cbx.dispatchEvent(e);
	}
	
	selectValue() {
		let rows = this.tbl.rows;
		let value = this.cbx.value;
		let sel = this.tbl.querySelector('.sel');
		if (sel) sel.className = '';
		this.row = -1;
		if (value == '') return;
		for (let i = 0; i < rows.length; i++) {
			if (value.localeCompare(rows[i].textContent, undefined, { sensitivity: 'accent' }) == 0) {
				rows[i].className = 'sel';
				rows[i].scrollIntoView({block: "center"});
				this.row = i;
				break;
			}
		}
	}
	
	show() {
		this.stopClick = true;
		if (this.isMobile) {
			this.lst.style.position = 'fixed';
			this.lst.style.left = '20px';
			this.lst.style.width = 'calc(100% - 40px)';
			if (this.lst.scrollHeight >= window.innerHeight) {
				this.lst.style.top = '20px';
				this.lst.style.height = 'calc(100% - 40px)';
			} else {
				this.lst.style.top = (window.innerHeight / 2 - this.lst.offsetHeight / 2) + 'px';
			}
		} else {
			let coords = this.cbx.getBoundingClientRect();
			this.lst.style.left = coords.left + window.pageXOffset + 'px'; //rect.left + 'px';
			this.lst.style.top = coords.bottom + window.pageYOffset + 'px';  //rect.bottom + 'px';
			this.lst.style.width = this.cbx.offsetWidth + this.bn.offsetWidth + 'px';
			if (this.lst.scrollHeight < 300) {
				this.lst.style.height = this.lst.scrollHeight + 'px';
			} else {
				this.lst.style.height = '300px';
			}
		}
		this.lst.style.zIndex = 10000;
		this.lst.style.visibility = 'visible';
		this.selectValue();
		if (!this.isMobile) this.cbx.focus();
		this.cbx.addEventListener('keydown', this.cbxKeyDownHandler);
		window.addEventListener('click', this.anyClickHandler);
	}
	
	hide() {
		window.removeEventListener('click', this.anyClickHandler);
		this.cbx.removeEventListener('keydown', this.cbxKeyDownHandler);
		this.lst.style.visibility = 'hidden';
		if (document.activeElement == document.body) this.cbx.focus();
		if (this.onHide) this.onHide(this);
	}
	
}

/////////////////////////////////////////////////////////////////////////////////////////
// Меню поля "изображение"
/////////////////////////////////////////////////////////////////////////////////////////

class ImageMenu {
	
	uploadClick = (ev) => {
		let fileInput = document.createElement('input');
		fileInput.type = 'file';
		fileInput.accept = 'image/jpeg,image/png,image/gif,image/bmp,image/tiff';
		fileInput.click();
		fileInput.onchange = (ev) => {
			let data = new FormData();
			data.append('file', ev.target.files[0]);
			data.append('fresh', this.freshValue);
			SendRequest('PUT', getCurrentUrl() + '&imgup=' + this.cid, data, (Request) => {
				if (Request.status == rcAjaxOk) {
					if (this.onChange) this.onChange(JSON.parse(Request.responseText));
				} else {
					showAjaxError(Request);
				}
			});
		}
	}

	viewClick = (ev) => {
		window.open(getCurrentUrl() + '&imgvw=' + this.cid, '_blank');
	}

	clearClick = (ev) => {
		SendRequest('POST', getCurrentUrl() + '&imgclr=' + this.cid, 'fresh=' + this.freshValue, (Request) => {
			if (Request.status == rcAjaxOk) {
				if (this.onChange) this.onChange(JSON.parse(Request.responseText));
			} else {
				showAjaxError(Request);
			}
		});
	}
	
	anyClickHandler = (ev) => {
		if (this.firstClick) this.firstClick = false
		else this.hide();
	}
	
	constructor (cid, freshValue) {
		this.cid = cid;
		this.freshValue = freshValue;
		this.img = document.getElementById('f' + cid);
		this.onChange = null;
		this.onHide = null;
		this.firstClick = true;
		
		this.mnu = document.createElement('div');
		this.mnu.style.position = 'absolute';
		this.mnu.style.left = this.img.style.left;
		this.mnu.style.top = this.img.style.top;
		this.mnu.style.zIndex = 10000;
		this.mnu.style.whiteSpace = 'nowrap';
		let hasSrc = this.img.getAttribute('src') != '/img/noimg.svg';
		let readOnly = this.img.hasAttribute('data-readonly');
		let noUpload = this.img.hasAttribute('data-noupload');
		
		this.mnu.innerHTML = '<button' +
			(!readOnly && !noUpload ? '' : ' disabled') +
			'><img width=25 height=25 src="/img/upload.svg"></button>&nbsp;<button' + 
			(hasSrc ? '' : ' disabled') +
			'><img width=25 height=25 src="/img/viewimg.svg"></button>&nbsp;<button' + 
			(hasSrc && !readOnly ? '' : ' disabled') + 
			'><img width=25 height=25 src="/img/delete.svg"></button>';
		let bns = this.mnu.getElementsByTagName('button');
		bns[0].onclick = this.uploadClick;
		bns[1].onclick = this.viewClick;
		bns[2].onclick = this.clearClick;
	}
	
	show() {
		this.img.parentNode.insertBefore(this.mnu, this.img.nextSibling);
		window.addEventListener('click', this.anyClickHandler);
	}
	
	hide() {
		this.mnu.remove();
		window.removeEventListener('click', this.anyClickHandler);
		if (this.onHide) this.onHide(this);
	}
}

/////////////////////////////////////////////////////////////////////////////////////////
// Меню поля "файл"
/////////////////////////////////////////////////////////////////////////////////////////

class FileMenu {
	
	uploadClick = (ev) => {
		let fileInput = document.createElement('input');
		fileInput.type = 'file';
		fileInput.click();
		fileInput.onchange = (ev) => {
			let data = new FormData();
			data.append('file', ev.target.files[0]);
			data.append('fresh', this.freshValue);
			SendRequest('PUT', getCurrentUrl() + '&flup=' + this.cid, data, (Request) => {
				if (Request.status == rcAjaxOk) {
					if (this.onChange) this.onChange(JSON.parse(Request.responseText));
				} else {
					showAjaxError(Request);
				}
			});
		}
	}

	downloadClick = (ev) => {
		SendRequest('POST', getCurrentUrl() + '&fldl=' + this.cid, 'fresh=' + this.freshValue, (Request) => {
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
			} else if (jsonObj.error) {
				alert(jsonObj.error);
			}
		});
	}

	clearClick = (ev) => {
		SendRequest('POST', getCurrentUrl() + '&flclr=' + this.cid, 'fresh=' + this.freshValue, (Request) => {
			if (Request.status == rcAjaxOk) {
				if (this.onChange) this.onChange(JSON.parse(Request.responseText));
			} else {
				showAjaxError(Request);
			}
		});
	}
	
	anyClickHandler = (ev) => {
		if (this.firstClick) this.firstClick = false
		else this.hide();
	}
	
	constructor (cid, freshValue) {
		this.cid = cid;
		this.freshValue = freshValue;
		this.fl = document.getElementById('f' + cid);
		this.bn = this.fl.nextSibling;
		this.onChange = null;
		this.onHide = null;
		this.firstClick = true;
		let hasValue = this.fl.value != '';
		let readOnly = this.fl.hasAttribute('file-readonly');
		
		this.mnu = document.createElement('div');
		this.mnu.style.visibility = 'hidden';
		this.mnu.style.whiteSpace = 'nowrap';
		this.bn.parentNode.insertBefore(this.mnu, this.bn.nextSibling);
		this.mnu.innerHTML = '<button' +
			(!readOnly ? '': ' disabled') +
			'><img width=25 height=25 src="/img/upload.svg"></button>&nbsp;<button' + 
			(hasValue ? '' : ' disabled') +
			'><img width=25 height=25 src="/img/download.svg"></button>&nbsp;<button' + 
			(hasValue && !readOnly ? '' : ' disabled') + 
			'><img width=25 height=25 src="/img/delete.svg"></button>';
		this.mnu.style.position = 'absolute';
		this.mnu.style.left = (parseInt(this.bn.style.left) + this.bn.offsetWidth / 2 - this.mnu.offsetWidth / 2) + 'px';
		this.mnu.style.top = (parseInt(this.bn.style.top) - this.mnu.offsetHeight) + 'px';
		this.mnu.style.zIndex = 10000;
		
		let bns = this.mnu.getElementsByTagName('button');
		bns[0].onclick = this.uploadClick;
		bns[1].onclick = this.downloadClick;
		bns[2].onclick = this.clearClick;
	}
	
	show() {
		this.mnu.style.visibility = 'visible';
		window.addEventListener('click', this.anyClickHandler);
	}
	
	hide() {
		this.mnu.remove();
		window.removeEventListener('click', this.anyClickHandler);
		if (this.onHide) this.onHide(this);
	}
}

/////////////////////////////////////////////////////////////////////////////////////////
// Меню кнопки "Дублировать"
/////////////////////////////////////////////////////////////////////////////////////////

class DuplicateMenu {
	constructor (bn) {
		this.bn = bn;
		//this.isMobile = window.matchMedia('(pointer:coarse)').matches;
		let mnu = document.createElement('div');
		mnu.className = 'dup-menu';
		mnu.innerHTML = '<button type=button class=dupbn>' + rsDuplicate + '</button>' +
			'<button type=button class=dupallbn>' + rsDuplicateAll + '</button>';
		mnu.style.position = 'absolute';
		mnu.style.visibility = 'hidden';
		document.body.appendChild(mnu);
		mnu.style.left = (bn.offsetLeft - mnu.offsetWidth / 2 + bn.offsetWidth / 2) + 'px';
		mnu.style.top = (bn.offsetTop + bn.offsetHeight + 4) + 'px';
		mnu.querySelector('.dupbn').addEventListener('click', this.bnClick);
		mnu.querySelector('.dupallbn').addEventListener('click', this.bnClick);
		this.mnu = mnu;
		this.onHide = null;
		this.onClick = null;
		this.firstClick = true;
	}
	
	bnClick = (ev) => {
		if (ev.currentTarget.className == 'dupbn') {
			if (this.onClick) this.onClick(getCurrentUrl() + '&dup')
		} else {
			if (this.onClick) this.onClick(getCurrentUrl() + '&dupall')
		}
	}
	
	anyClickHandler = (ev) => {
		if (this.firstClick) this.firstClick = false
		else this.hide();
	}
	
	show() {
		this.mnu.style.visibility = 'visible';
		window.addEventListener('click', this.anyClickHandler);
	}
	
	hide() {
		this.mnu.remove();
		window.removeEventListener('click', this.anyClickHandler);
		if (this.onHide) this.onHide(this);
	}
}


/////////////////////////////////////////////////////////////////////////////////////////
// Календарь поля "дата"
/////////////////////////////////////////////////////////////////////////////////////////

class Calendar {

	constructor(inp) {
		this.inp = inp;
		this.isMobile = window.matchMedia('(pointer:coarse)').matches;
		this.onHide = null;
		if (inp.value != '') {
			this.date = parseDate(inp.value);
			if (!this.date) this.date = new Date();
		} else 
			this.date = new Date();
	}
	
	itemClick = (ev) => {
		let d, m, y;
		let div = ev.currentTarget.parentNode;
		let el = div.querySelector('.sel');
		if (el) el.classList.remove('sel');
		ev.currentTarget.classList.add('sel');
		d = this.daysEl.querySelector('.sel').innerHTML.substr(-2).trim();
		m = this.monthsEl.querySelector('.sel').innerHTML.substr(-2).trim();
		y = this.yearsEl.querySelector('.sel').innerHTML;
		let dt = new Date(y, m - 1, d);
		if (dt && dt.getDate() == d) {
			this.date = dt;
		} else {
			this.date = false;
		}
		this.updateDateEl();
		if (div.className != 'days') this.updateDays(m, y);
	}
	
	itemDblClick = (ev) => {
		if (this.date) {
			this.setValue();
			this.hide();
		}
	}
	
	calBnClick = (ev) => {
		let bn = ev.currentTarget;
		if (bn.className == 'calbntoday') {
			this.date = new Date;
			this.updateDays(this.date.getMonth()+1, this.date.getFullYear());
			this.selectDate();
		} else if (bn.className == 'calbnclear') {
			this.clearValue();
			this.hide();
		} else if (bn.className == 'calbnok' && this.date) {
			this.setValue();
			this.hide();
		} else if (bn.className == 'calbncancel') {
			this.hide();
		}
	}
	
	anyClickHandler = (ev) => {
		if (this.stopClick) this.stopClick = false
		else if (!isContained(this.cal, ev.target)) this.hide();
	}
	
	changeBounds = () => {
		this.cal.style.height = '';
		this.itemsEl.style.height = this.monthsEl.scrollHeight + 'px';
		if (this.cal.offsetHeight > window.innerHeight) {
			this.cal.style.height = 'calc(100% - 40px)';
			this.itemsEl.style.height = 'calc(100% - ' + (this.dateEl.offsetHeight + this.cal.querySelector('.calbtns').offsetHeight) + 'px)';
		}
		this.cal.style.left = 'calc(50% - ' + this.cal.offsetWidth / 2 + 'px)';
		this.cal.style.top = 'calc(50% - ' + this.cal.offsetHeight / 2 + 'px)';
		this.selectDate();
	}
	
	setValue() {
		this.inp.value =  formatDate(this.date); //this.date.toLocaleDateString();
		let e = new Event('change');
		this.inp.dispatchEvent(e);
	}
	
	clearValue() {
		this.inp.value = '';
		let e = new Event('change');
		this.inp.dispatchEvent(e);
	}
	
	updateDateEl() {
		if (this.date) this.dateEl.innerHTML = rsWeekNamesBrief[this.date.getDay()] + ', ' + formatDate(this.date)  //this.date.toLocaleDateString('RU-ru')
		else this.dateEl.innerHTML = rsInvalidDate;
	}
	
	updateDays(m, y) {
		let beginMonth = new Date(y, m - 1, 1);
		let weekDay = beginMonth.getDay();
		for (let i = 1; i <= 31; i++) {
			let el = this.daysEl.children[i - 1];
			el.innerHTML = rsWeekNamesBrief[weekDay] + ' ' + i;
			//if (el.className != 'sel') {
				if (weekDay == 0 || weekDay == 6) el.classList.add('off')
				else el.classList.remove('off');
			//}
			weekDay++;
			if (weekDay > 6) weekDay = 0;
		}
	}
	
	selectItem(div, idx) {
		let el = div.querySelector('.sel');
		if (el) el.classList.remove('sel');
		el = div.children[idx];
		el.classList.add('sel');
		el.scrollIntoView({block: "center"});
	}
	
	selectDate() {
		if (!this.date) return;
		
		let d = this.date.getDate(), m = this.date.getMonth() + 1, y = this.date.getFullYear();
		
		this.selectItem(this.daysEl, d-1);
		this.selectItem(this.monthsEl, m-1);
		this.selectItem(this.yearsEl, y-1900);
		this.updateDateEl();
	}
	
	hide() {
		this.cal.remove();
		if (this.isMobile) window.removeEventListener('resize', this.changeBounds);
		if (this.onHide) this.onHide(this);
	}
	
	show() {
		this.stopClick = true;
		this.cal = document.createElement('div');
		this.cal.className = 'calendar';
		this.cal.style = 'background-color: white; border: solid 1px black; z-index: 10000; ';
		document.body.appendChild(this.cal);
		this.cal.innerHTML = '<div class=date>12.12.2012</div><div class=calitems><div class=days></div><div class=months></div><div class=years></div></div>' +
			'<div class=calbtns><button class=calbntoday type=button><img src="/img/point.svg"></button><button class=calbnclear type=button><img src="/img/delete.svg"></button>' +
			'<button class=calbnok type=button><img src="/img/ok.svg"></button><button class=calbncancel type=button><img src="/img/cancel.svg"></button></div>';
		this.dateEl = this.cal.querySelector('.date');
		this.itemsEl = this.cal.querySelector('.calitems');
		this.daysEl = this.cal.querySelector('.days');
		this.monthsEl = this.cal.querySelector('.months');
		this.yearsEl = this.cal.querySelector('.years');

		let btns, el, i;
		
		btns = this.cal.querySelectorAll('.calbtns button');
		for (i = 0; i < btns.length; i++) 
			btns[i].addEventListener('click', this.calBnClick);
		
		for (i = 1; i <= 31; i++) {
			el = document.createElement('span');
			el.addEventListener('click', this.itemClick);
			el.addEventListener('dblclick', this.itemDblClick);
			this.daysEl.appendChild(el);
		}

		this.updateDays(this.date.getMonth()+1, this.date.getFullYear());

		for (i = 1; i <= 12; i++) {
			el = document.createElement('span');
			el.innerHTML = rsMonthNamesBrief[i-1] + ' ' + i;
			el.addEventListener('click', this.itemClick);
			el.addEventListener('dblclick', this.itemDblClick);
			this.monthsEl.appendChild(el);
		}

		for (i = 1900; i <= 2100; i++) {
			el = document.createElement('span');
			el.innerHTML = i;
			el.addEventListener('click', this.itemClick);
			el.addEventListener('dblclick', this.itemDblClick);
			this.yearsEl.appendChild(el);
		}
		
		if (this.isMobile) {
			this.cal.style.position = 'fixed';
			this.changeBounds();
			window.addEventListener('resize', this.changeBounds);
		} else {
			this.cal.style.position = 'absolute';
			let coords = this.inp.getBoundingClientRect();
			this.cal.style.left = coords.left + window.pageXOffset + 'px'; 
			this.cal.style.top = coords.bottom + window.pageYOffset + 'px'; 
			this.itemsEl.style.height = this.monthsEl.scrollHeight + 'px';
		}
		this.selectDate();
		window.addEventListener('click', this.anyClickHandler);
	}
}

/////////////////////////////////////////////////////////////////////////////////////////
// Окно выбора времени
/////////////////////////////////////////////////////////////////////////////////////////

class TimeSelector {
	
	constructor(inp) {
		this.inp = inp;
		this.isMobile = window.matchMedia('(pointer:coarse)').matches;
		this.onHide = null;
		if (inp.value != '') {
			this.time = parseTime(inp.value);
			if (!this.time) this.time = new Date();
		} else {
			this.time = new Date();
		}
	}
	
	changeBounds = () => {
		this.tms.style.height = '';
		this.partsEl.style.height = this.minutesEl.scrollHeight + 'px';
		if (this.tms.offsetHeight > window.innerHeight) {
			this.tms.style.height = 'calc(100% - 40px)';
			this.partsEl.style.height = 'calc(100% - ' + (this.timeEl.offsetHeight + this.tms.querySelector('.timebtns').offsetHeight) + 'px)';
		}
		this.tms.style.left = 'calc(50% - ' + this.tms.offsetWidth / 2 + 'px)';
		this.tms.style.top = 'calc(50% - ' + this.tms.offsetHeight / 2 + 'px)';
	}
	
	calBnClick = (ev) => {
		let bn = ev.currentTarget;
		if (bn.className == 'timebnnow') {
			this.time = new Date();
			this.selectTime();
		} else if (bn.className == 'timebnclear') {
			this.clearValue();
			this.hide();
		} else if (bn.className == 'timebnok' && this.time) {
			this.setValue();
			this.hide();
		} else if (bn.className == 'timebncancel') {
			this.hide();
		}
	}
	
	itemClick = (ev) => {
		let h, m;
		let div = ev.currentTarget.parentNode;
		let el = div.querySelector('.sel');
		if (el) el.classList.remove('sel');
		ev.currentTarget.classList.add('sel');
		h = this.hoursEl.querySelector('.sel').innerHTML;
		m = this.minutesEl.querySelector('.sel').innerHTML;
		this.time.setHours(h);
		this.time.setMinutes(m);
		this.updateTimeEl();
	}
	
	itemDblClick = (ev) => {
		this.setValue();
		this.hide();
	}
	
	anyClickHandler = (ev) => {
		if (this.stopClick) this.stopClick = false
		else if (!isContained(this.tms, ev.target)) this.hide();
	}
	
	updateTimeEl() {
		this.timeEl.innerHTML = formatTime(this.time).slice(0, -3); //  this.time.toLocaleTimeString().slice(0, -3);
	}
	
	setValue() {
		this.time.setSeconds(0);
		this.inp.value = formatTime(this.time).slice(0, -3); // this.time.toLocaleTimeString();
		let e = new Event('change');
		this.inp.dispatchEvent(e);
	}
	
	clearValue() {
		this.inp.value = '';
		let e = new Event('change');
		this.inp.dispatchEvent(e);
	}
	
	selectItem(div, idx) {
		let el = div.querySelector('.sel');
		if (el) el.classList.remove('sel');
		el = div.children[idx];
		el.classList.add('sel');
		el.scrollIntoView({block: "center"});
	}
	
	selectTime() {
		if (!this.time) return;
		
		if (this.time.getMinutes() % 5 != 0) this.time.setMinutes(Math.round(this.time.getMinutes() / 5) * 5);
		let h = this.time.getHours(), m = this.time.getMinutes();
		
		this.selectItem(this.hoursEl, h);
		this.selectItem(this.minutesEl, m / 5);
		this.updateTimeEl();
	}
	
	hide() {
		this.tms.remove();
		if (this.isMobile) window.removeEventListener('resize', this.changeBounds);
		if (this.onHide) this.onHide(this);
	}
	
	show() {
		this.stopClick = true;
		this.tms = document.createElement('div');
		this.tms.className = 'timeselector';
		this.tms.style = 'background-color: white; border: solid 1px black; z-index: 10000; ';
		document.body.appendChild(this.tms);
		this.tms.innerHTML = '<div class=time>12:12</div><div class=timeparts><div class=hours></div><div class=minutes></div></div>' +
			'<div class=timebtns><button class=timebnnow type=button><img src="/img/point.svg"></button><button class=timebnclear type=button><img src="/img/delete.svg"></button>' +
			'<button class=timebnok type=button><img src="/img/ok.svg"></button><button class=timebncancel type=button><img src="/img/cancel.svg"></button></div>';
		this.timeEl = this.tms.querySelector('.time');
		this.partsEl = this.tms.querySelector('.timeparts');
		this.hoursEl = this.tms.querySelector('.hours');
		this.minutesEl = this.tms.querySelector('.minutes');
		
		let btns, el, i;
		
		btns = this.tms.querySelectorAll('.timebtns button');
		for (i = 0; i < btns.length; i++) 
			btns[i].addEventListener('click', this.calBnClick);
		
		for (i = 0; i <= 23; i++) {
			el = document.createElement('span');
			el.innerHTML = (i < 10 ? '0' + i : i);
			el.addEventListener('click', this.itemClick);
			el.addEventListener('dblclick', this.itemDblClick);
			this.hoursEl.appendChild(el);
		}

		for (i = 0; i <= 59; i += 5) {
			el = document.createElement('span');
			el.innerHTML = (i < 10 ? '0' + i : i);
			el.addEventListener('click', this.itemClick);
			el.addEventListener('dblclick', this.itemDblClick);
			this.minutesEl.appendChild(el);
		}
		
		if (this.isMobile) {
			this.tms.style.position = 'fixed';
			this.changeBounds();
			window.addEventListener('resize', this.changeBounds);
		} else {
			this.tms.style.position = 'absolute';
			let coords = this.inp.getBoundingClientRect();
			this.tms.style.left = coords.left + window.pageXOffset + 'px'; 
			this.tms.style.top = coords.bottom + window.pageYOffset + 'px';  
			this.partsEl.style.height = this.minutesEl.scrollHeight + 'px';
		}
		this.selectTime();
		window.addEventListener('click', this.anyClickHandler);
	}
	
}


/////////////////////////////////////////////////////////////////////////////////////////
// Окно сообщения
/////////////////////////////////////////////////////////////////////////////////////////

class MessageBox {
	getBnCaption(bn) {
		if (bn == 'yes') return rsYes
		else if (bn == 'no') return rsNo
		else if (bn == 'ok') return rsOk
		else if (bn == 'cancel') return rsCancel
		else if (bn == 'abort') return rsAbort
		else if (bn == 'retry') return rsRetry
		else if (bn == 'ignore') return rsIgnore
		else if (bn == 'all') return rsAll
		else if (bn == 'noToAll') return rsNoToAll
		else if (bn == 'yesToAll') return rsYesToAll
		else if (bn == 'help') return rsHelp
		else if (bn == 'close') return rsClose
		else return '';
	}
	
	buttonClick = (ev) => {
		this.box.remove();
		if (this.onButtonClick) this.onButtonClick(ev.currentTarget.id);
	}
	
	constructor(msgInfo) {
		this.onButtonClick = null;
		this.box = document.createElement('dialog');
		this.box.className = 'msgbox';
		let boxHtml = '<div class=msgtitle>' + msgInfo.title + '</div><div class=msgtext>' + msgInfo.msg + 
			'</div><div class=msgbns>';
		for (let bn of msgInfo.buttons) {
			boxHtml += '<button id=' + bn + 'bn>' + this.getBnCaption(bn) + '</button>';
		}
		boxHtml += '</div>';
		this.box.innerHTML = boxHtml;
		this.box.style.position = 'fixed';
		this.box.style.zIndex = 3;
		document.body.appendChild(this.box);
		this.box.style.top = '0px';
		
		let buttons = this.box.getElementsByTagName('button');
		for (let bn of buttons) {
			bn.addEventListener('click', this.buttonClick);
		}
		
		this.box.showModal();
	}
}

/////////////////////////////////////////////////////////////////////////////////////////
// Занавес
/////////////////////////////////////////////////////////////////////////////////////////

class Curtain {
	curtainKeyDown = (ev) => {
		if (ev.code == 'Tab' || ev.code == 'Enter' || ev.code == 'Space') ev.preventDefault();
	}
	
	show() {
		document.body.appendChild(this.el);
		document.body.addEventListener('keydown', this.curtainKeyDown);
		this.timer = setTimeout(() => {
			document.body.appendChild(this.loader);
		}, 3000);
	}
	
	hide() {
		clearTimeout(this.timer);
		this.el.remove();
		this.loader.remove();
		document.body.removeEventListener('keydown', this.curtainKeyDown);
	}
	
	constructor() {
		this.el = document.createElement('div');
		this.el.style = 'position: fixed; top: 0px; left: 0px; width: 100%; height: 100%; background-color: black; opacity: 0; z-index: 20000;';
		this.loader = document.createElement('div');
		this.loader.className = 'loader';
	}
}

/////////////////////////////////////////////////////////////////////////////////////////
// Глобальные функции для работы с полями форм и фильтров
/////////////////////////////////////////////////////////////////////////////////////////

let	cbxList = null;
let	cal = null;
let tms = null;
let curtain = null;
let curtainImg = null;
let curtainCounter = 0;

function showList() {
	let cbx = (event.currentTarget.tagName == 'BUTTON' ? event.currentTarget.previousSibling : event.currentTarget);
	let cid;
	if (cbx.hasAttribute('id')) 
		cid = cbx.id.substr(1)
	else
		cid = cbx.parentNode.parentNode.parentNode.dataset.id;

	if (cbxList && cbxList.cbx == cbx) {
		if (cbxList.lst.style.visibility == 'visible') { 
			if (document.activeElement == cbxList.bn) cbxList.cbx.focus();
			cbxList.hide();
		}
	} else {
		cbxList = new ListCbx(cbx, cid);
		cbxList.show();
		cbxList.onHide = function (that) {
			if (cbxList == that) cbxList = null;
		}
	}
}

function showFixedList() {
	let cbx = (event.currentTarget.tagName == 'BUTTON' ? event.currentTarget.previousSibling : event.currentTarget);

	if (cbxList && cbxList.cbx == cbx) {
		if (cbxList.lst.style.visibility == 'visible') { 
			if (document.activeElement == cbxList.bn) cbxList.cbx.focus();
			cbxList.hide();
		}
	} else {
		cbxList = new FixedListCbx(cbx);
		cbxList.show();
		cbxList.onHide = function (that) {
			if (cbxList == that) cbxList = null;
		}
	}
}

function showCalendar(bn) {
	if (cal && bn.previousSibling == cal.inp) cal.hide()
	else {
		cal = new Calendar(bn.previousSibling);
		cal.onHide = (that) => {
			if (that == cal) cal = null;
		}
		cal.show();
	}
}

function showTime(bn) {
	if (tms && bn.previousSibling == tms.inp) tms.hide()
	else {
		tms = new TimeSelector(bn.previousSibling);
		tms.onHide = (that) => {
			if (that == tms) tms = null;
		}
		tms.show();
	}
}

function lcbxKeyDown() {
	if (event.code == 'ArrowDown') {
		event.currentTarget.nextSibling.click();
	} else if (event.code == 'Delete' || event.code == 'Backspace') {
		let cbx = event.currentTarget;
		cbx.value = '';
		cbx.previousSibling.value = "";
		let e = new Event('change');
		cbx.previousSibling.dispatchEvent(e);
	}
}

function cbxKeyDown() {
	if (event.code == 'ArrowDown') {
		event.currentTarget.nextSibling.click();
	}
}

function fixedCbxKeyDown() {
	if (event.code == 'ArrowDown' && event.currentTarget.nextSibling.nextSibling.style.visibility == 'hidden') {
		event.currentTarget.nextSibling.click();
	}
}

function menuClick(el) {
	let sb = document.getElementById('sidebar');
	sb.style.display = 'block';
}

function closeSidebarClick(el) {
	let sb = document.getElementById('sidebar');
	sb.style.display = 'none';
}

function showCurtain() {
	if (curtainCounter == 0) {
		curtain = new Curtain();
		curtain.show();
	}
	curtainCounter++;
}

function hideCurtain() {
	curtainCounter--;
	if (curtain && curtainCounter == 0) {
		curtain.hide();
		curtain = null;
	}
}

function closeDebug() {
	SendRequest('POST', '?closedbg', '', null);
	document.getElementById('debug-block').style.display = 'none';
}

function clearDebug() {
	SendRequest('POST', '?cleardbg', '', null);
	document.getElementById('debug-body').innerHTML = '';
}

