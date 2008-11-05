var codeItemName = 'codeitem';

var standardClass = 'standard';
var errorClass = 'error';
var endpoint1Class = 'endpoint1';
var endpoint2Class = 'endpoint2';
var alertClass = 'alert';
var unboundvarClass = 'unboundvar';
var currentErrorClass = 'currenterror';
var otherErrorClass = 'othererror';

var errorListId = 'errorlist';
var errorId = 'error';
var resetId = 'reset';

var errors = new Array();
var codeItems = new Object();

function setClass(id, clazz) {
  if (id in codeItems)
    codeItems[id].className = clazz;
  else
    alert(id + ' is an undefined code item.');
}

function createTextNodeWithClass(className, text) {
  var span = document.createElement('span');
  span.className = className;
  span.appendChild(document.createTextNode(text));
  return span;
}

function Clash() {
  this.display = function() {
    for(var i in this.errorlocs)
      setClass(this.errorlocs[i], errorClass)
    setClass(this.endpoint1, endpoint1Class);
    setClass(this.endpoint2, endpoint2Class);
  }
}

function ConstructorClash(ty1, ty2, endpoint1, endpoint2, errorlocs) {
  this.ty1 = ty1;
  this.ty2 = ty2;
  this.endpoint1 = endpoint1;
  this.endpoint2 = endpoint2;
  this.errorlocs = errorlocs;
  this.message = function() {
    var span = document.createElement('span');
    span.appendChild(document.createTextNode("Type constructor clash: "));
    span.appendChild(createTextNodeWithClass(endpoint1Class, this.ty1));
    span.appendChild(document.createTextNode(" versus "));
    span.appendChild(createTextNodeWithClass(endpoint2Class, this.ty2));
    return span;
  }
}
ConstructorClash.prototype = new Clash;

function ArityClash(ar1, ar2, endpoint1, endpoint2, errorlocs) {
  this.ar1 = ar1;
  this.ar2 = ar2;
  this.endpoint1 = endpoint1;
  this.endpoint2 = endpoint2;
  this.errorlocs = errorlocs;
  this.message = function() {
    var span = document.createElement('span');
    span.appendChild(document.createTextNode("Arity clash: "));
    span.appendChild(createTextNodeWithClass(endpoint1Class, this.ar1));
    span.appendChild(document.createTextNode(" versus "));
    span.appendChild(createTextNodeWithClass(endpoint2Class, this.ar2));
    return span;
  }
}
ArityClash.prototype = new Clash;

function UnboundVar(varName, varLoc) {
  this.varName = varName;
  this.varLoc = varLoc;
  this.message = function() {
    var span = document.createElement('span');
    span.appendChild(document.createTextNode("Unbound variable: "));
    span.appendChild(createTextNodeWithClass(unboundvarClass, this.varName));
    return span;
  }
  this.display = function() {
	setClass(this.varLoc, unboundvarClass);
  }
}

function LocalError(desc, loc) {
  this.desc = desc;
  this.loc = loc;
  this.message = function() {
    var span = document.createElement('span');
    span.appendChild(document.createTextNode(this.desc));
    return span;
  }
  this.display = function() {
    setClass(this.loc, errorClass);
  }
}

function CircularType(ty1, ty2, opt_endpoint1, opt_endpoint2, errorlocs) {
  this.ty1 = ty1;
  this.ty2 = ty2;
  this.opt_endpoint1 = opt_endpoint1;
  this.opt_endpoint2 = opt_endpoint2;
  this.errorlocs = errorlocs;
  this.message = function() {
    var span = document.createElement('span');
    span.appendChild(document.createTextNode("Circular type: "));
    span.appendChild(createTextNodeWithClass(opt_endpoint1 != null ? endpoint1Class : '', this.ty1));
    span.appendChild(document.createTextNode(" and "));
    span.appendChild(createTextNodeWithClass(opt_endpoint2 != null ? endpoint2Class : '', this.ty2));
    return span;
  }
  this.display = function() {
    for(var i in errorlocs)
      setClass(errorlocs[i], errorClass)
    if(opt_endpoint1 != null)
      setClass(opt_endpoint1, endpoint1Class);
    if(opt_endpoint2 != null)
      setClass(opt_endpoint2, endpoint2Class);
  }
}

function init() {
  var spans = document.getElementsByName(codeItemName);
  for(var i = 0; i < spans.length; i++) 
    codeItems[spans[i].getAttribute('id')] = spans[i]

  for(var spanId in codeItems)
    codeItems[spanId].className = standardClass;

  var errorlist = document.getElementById(errorListId);
  for(var i in errors) {
    var sp = document.createElement('span');
	sp.setAttribute('onclick', 'javascript:error('+i+')');
	sp.setAttribute('id', errorId + i);
	sp.setAttribute('class', otherErrorClass);
    sp.appendChild(errors[i].message());
    var li = document.createElement('li');
    li.appendChild(sp);
    errorlist.appendChild(li);
  }
  reset();
}

function reset() {
  for(var spanId in codeItems)
    codeItems[spanId].className = standardClass;
  for(var i in errors)
    document.getElementById(errorId + i).className = otherErrorClass;
  document.getElementById(resetId).className = currentErrorClass;
}

function registerErrors(errs) {
  errors = errs;
}

function error(n) {
  reset();
  document.getElementById(resetId).className = otherErrorClass;
  errors[n].display();
  document.getElementById(errorId + n).className = currentErrorClass;
}
