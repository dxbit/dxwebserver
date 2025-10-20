function formSubmit(fm) {
	let items = fm.querySelectorAll('div.filter-item');
	let prd = document.getElementById('prd');
	if (prd) prdVal = prd.value
	else prdVal = 0;
	let params = [{period: prdVal}];
	for (let item of items) {
		let id = item.dataset.id;
		let div = item.children[0];
		let fl = { };
		fl.field = id;
		fl.not = (div.children[1].checked ? 1 : 0);
		fl.nul = (div.children[3].checked ? 1 : 0);
		fl.vals = [];
		for (let i = 1; i < item.children.length; i++) {
			let vl = { };
			let valEl = item.children[i].firstChild.firstChild;
			if (valEl.tagName == 'INPUT') {
				if (valEl.className == 'one') {
					vl.value = valEl.value;
					vl.valueEnd = valEl.nextSibling.value;
				} else if (valEl.className == 'onebn') {
					vl.value = valEl.value;
					vl.valueEnd = valEl.nextSibling.nextSibling.value;
				} else {
					vl.value = encodeURIComponent(valEl.value);
				}
			} else if (valEl.tagName == 'SELECT') {
				vl.value = encodeURIComponent(valEl.value);
			}
			fl.vals.push(vl);
		}
		params.push(fl);
	}
	SendRequest('POST', getCurrentUrl() + '&exec', JSON.stringify(params), (Request) => {
		if (Request.status == rcAjaxOk) {
			let args = new URLSearchParams(getCurrentUrl().slice(1));
			reloadPage();//location.reload();
		} else {
			showAjaxError(Request);
		}
	});
}

function clearValue(valEl) {
	valEl.value = '';
	if (valEl.className == 'one' || valEl.type == 'hidden') 
		valEl.nextSibling.value = ''
	else if (valEl.className == 'onebn')
		valEl.nextSibling.nextSibling.value = '';
}

function delValue(bn) {
	if (bn.parentNode.parentNode.children.length > 2) {
		bn.parentNode.remove();
	} else {
		let valEl = bn.parentNode.firstChild.firstChild;
		clearValue(valEl);
	}
}

function addValue(bn) {
	let divField = bn.parentNode.cloneNode(true);
	let valEl = divField.firstChild.firstChild;
	clearValue(valEl);
	bn.parentNode.after(divField);
}

function templateClick(el) {
	SendRequest('POST', getCurrentUrl() + '&print=' + el.dataset.index, '', (Request) => {
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

function periodsClick(index) {
	let el = document.getElementById('periods');
	if (el.style.display == '') el.style.display = 'none'
	else el.style.display = '';
}

function printClick() {
	let el = document.getElementById('templates');
	if (el.style.display == '') el.style.display = 'none'
	else el.style.display = '';
}

function tableClick() {
	let el = event.target;
	if (el.tagName == 'IMG' && el.className == 'del') {
		if (confirm(rsDeleteRecMsg)) {
			let a = el.nextSibling;
			SendRequest('POST', a.href + '&formdel', '', (Request) => {
				if (Request.status == rcAjaxOk) {
					let le = document.querySelectorAll('a.le');
					for (let i = 0; i < le.length; i++) {
						if (le[i].href == a.href) {
							le[i].remove();
							break;
						}
					}
					
					let row = el.closest('tr');
					row.remove();
				} else {
					showAjaxError(Request);
				}
			});
		}
	}
}

function headerClick() {
	let el = event.currentTarget;
	let params = 'fid=' + el.dataset.fid;
	if (el.dataset.order) {
		params += '&order=' + el.dataset.order;
	}
	if (event.ctrlKey) params += '&add=1';
	SendRequest('POST', getCurrentUrl() + '&sort', params, (Request) => {
		if (Request.status == rcAjaxOk) {
			reloadPage();//location.reload();
		} else {
			alert(rsSomethingWrong);
		}
	});
}

function formAdd(fmId) {
	SendRequest('POST', '?formadd', 'id=' + fmId, (Request) => {
		if (Request.status == rcAjaxOk) gotoUrl(Request.responseText); 
		else showAjaxError(Request);
	});
}

// После перехода по ссылке и возврате назад страница не обновляется (перестало работать). Поэтому делаем такой трюк - отправляем фиктивный POST-запрос.
function formEdit() {
	let el = event.currentTarget;
	event.preventDefault();
	SendRequest('POST', '?formedit', '', (Request) => {
		gotoUrl(el.href);
	});
}

function reportFetch() {
	let tbl = document.querySelector('div.grid table');
	let rows = tbl.getElementsByTagName('TR');
	rows[rows.length - 1].remove();
	let rowCount = rows.length - 1;
	SendRequest('POST', getCurrentUrl() + '&rpfetch', 'skip=' + rowCount + '&fresh=' + tbl.dataset.freshvalue, (Request) => {
		if (Request.status == rcAjaxOk) {
			let lastRow = rows[rows.length - 1];
			lastRow.insertAdjacentHTML('afterend', Request.responseText);
		} else {
			showAjaxError(Request);
		}
	});
}

/*function scrollWindow() {
	let body = document.body;
	if (window.pageYOffset + window.innerHeight == body.scrollHeight) {
		let bn = document.querySelector('.gridbns button');
		if (bn) bn.click();
	}
}*/

function bodyLoad() {
	let item = document.querySelector('div.sidebar a.sel');
	if (item) item.scrollIntoView({block: "center"});
	//window.addEventListener('scroll', scrollWindow);
}