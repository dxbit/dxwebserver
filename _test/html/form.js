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
	let params = 'id=' + el.dataset.fid;
	if (el.dataset.order) {
		params += '&order=' + el.dataset.order;
	}
	if (event.ctrlKey) params += '&add=1';
	SendRequest('POST', getCurrentUrl() + '&sort', params, (Request) => {
		if (Request.status == rcAjaxOk) {
			reloadPage();//location.reload();
		} else {
			showAjaxError(Request);
		}
	});
}

function filterClick(el) {
	let flt = document.getElementById('filters');
	if (flt.style.display == 'none' || flt.style.display == '')
		flt.style.display = 'block'
	else 
		flt.style.display = 'none';
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

function bodyLoad() {
	let item = document.querySelector('div.sidebar a.sel');
	if (item) item.scrollIntoView({block: "center"});
	
	let dbgBody = document.getElementById('debug-body');
	if (dbgBody && dbgBody.hasChildNodes()) dbgBody.lastChild.scrollIntoView();
}