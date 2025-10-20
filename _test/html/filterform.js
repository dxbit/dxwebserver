function goBack() {
	let url = null;
	if (document.referrer != '') url = new URL(document.referrer);
	showCurtain();
	if (url == null || url.pathname.toLowerCase() != location.pathname.toLowerCase() || url.search == '?login') {
		let params = new URLSearchParams(getCurrentUrl().slice(1));
		if (params.has('fm') && params.has('pg')) {
			location.href = '?fm=' + params.get('fm') + '&pg=' + params.get('pg');
		}
	} else {
		window.history.back();
	}
}

function formSubmit(fm) {
	let items = fm.querySelectorAll('div.filter-item');
	let params = [];
	for (let item of items) {
		let id = item.dataset.id;
		let div = item.children[0];
		let fl = { };
		fl.field = id;
		fl.not = (div.children[2].checked ? 1 : 0);
		fl.nul = (div.children[4].checked ? 1 : 0);
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
	SendRequest('POST', getCurrentUrl() + '&fltok', JSON.stringify(params), (Request) => {
		if (Request.status == rcAjaxOk) {
			let args = new URLSearchParams(getCurrentUrl().slice(1));
			if (args.has('fm'))
				gotoUrl('?fm=' + args.get('fm') + '&pg=1');
		} else
			showAjaxError(Request);
	});
}

function clearValue(valEl) {
	valEl.value = '';
	if (valEl.className == 'one' || valEl.type == 'hidden') 
		valEl.nextSibling.value = ''
	else if (valEl.className == 'onebn')
		valEl.nextSibling.nextSibling.value = ''
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

function delField(bn) {
	bn.parentNode.parentNode.remove();
}

function addField(sel) {
	let idx = sel.selectedIndex;
	let fitem = document.createElement('div');
	fitem.className = 'filter-item';
	fitem.dataset.id = sel.value;
	fitem.innerHTML = '<div class=field><button class=fltbn type=button onclick="delField(this)"><img src="/img/delete.svg"></button><span>' + sel.options[idx].text + '</span><input type=checkbox><span>' + rsNot + 
		'</span><input type=checkbox><span>' + rsEmpty + '</span></div>';
	let val = document.createElement('div');
	val.className = 'value';
	val.innerHTML = '<button class=fltbn type=button onclick="delValue(this)"><img src="/img/delete.svg"></button><button class=fltbn type=button onclick="addValue(this)"><img src="/img/add.svg"></button></div>';
	let ctrls = document.getElementById('ctrls');
	let ctrl = ctrls.children[idx-1].cloneNode(true);
	val.insertBefore(ctrl, val.firstChild);
	fitem.appendChild(val);
	document.getElementById('filter').appendChild(fitem);
	sel.value = '';
}

function bodyLoad() {
	let item = document.querySelector('div.sidebar a.sel');
	if (item) item.scrollIntoView({block: "center"});
}