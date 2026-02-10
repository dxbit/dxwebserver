function formSubmit(fm) {
	let msg = document.getElementById('msg');
	let bn = document.getElementById('submit'); 
	let loader = document.getElementById('loader'); 
	msg.className = '';
	msg.innerHTML = rsConnecting;
	bn.className = 'hide';
	loader.className = 'loader';
	fm.className = '';
	fm.user.disabled = true;
	fm.pwd.disabled = true;
	
	let w = window.localStorage.getItem('clientWidth');
	let h = window.localStorage.getItem('clientHeight');
	let params = 'user=' + encodeURIComponent(fm.user.value) +  '&pwd=' + encodeURIComponent(fm.pwd.value);
	if (w && h) params = params + '&w=' + w + '&h=' + h;
	SendRequest('POST', location.pathname + '?login', params, (Request) => {
		bn.className = '';
		loader.className = 'hide';
		fm.user.disabled = false;
		fm.pwd.disabled = false;

		if (Request.status != rcAjaxOk && Request.status != rcAjaxError) {
			alert(rsSomethingWrong);
			msg.className = 'err';
			msg.innerHTML = rsServerError;
		}
		else if (Request.status == rcAjaxOk) {
			let jsonObj = JSON.parse(Request.responseText);
			gotoUrl(jsonObj.url);
		} else if (Request.status == rcAjaxError) {
			let jsonObj = JSON.parse(Request.responseText);
			if (jsonObj.code == 7) {
				msg.innerHTML = rsIncorrectPwd;
				fm.pwd.focus();
			} else {
				alert(jsonObj.error);
				msg.innerHTML = rsServerError;
			}
			fm.className = 'err';
			msg.className = 'err';
		}
	}, false, false);
}

window.onload = function() {
	if (document.getElementById('msg').hasAttribute('data-connecting'))
		setTimeout(function() {
			window.location.reload();
		}, 5000);
}