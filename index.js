(()=>{var Le={compose:function(n){return function(t){return function(r){return n(t(r))}}}};var Z=function(n){return n.identity},an={identity:function(n){return n},Semigroupoid0:function(){return Le}};var Bn=function(n){return function(t){return function(r){return n(r)(t)}}},j=function(n){return function(t){return n}};var Ja=function(n){return function(t){for(var r=t.length,e=new Array(r),o=0;o<r;o++)e[o]=n(t[o]);return e}};var u=function(n){return n.map},Gn=function(n){return function(t){return function(r){return u(n)(r)(t)}}};var Mt={map:Ja};var _n=function(n){return n.apply};var Re=function(n){return function(t){return function(r){return _n(n)(u(n.Functor0())(j(Z(an)))(t))(r)}}};var i=function(n){return n.pure};var ar=function(n){return function(t){return function(r){if(t)return r;if(!t)return i(n)(void 0);throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): "+[t.constructor.name,r.constructor.name])}}},zr=function(n){return function(t){return function(r){return _n(n.Apply0())(i(n)(t))(r)}}};var Wn=function(n){return n.discard};var c=function(n){return n.bind},Nn=function(n){return Bn(c(n))},Io=function(n){return function(t){return function(r){return function(e){return Nn(n)(t)(r(e))}}}};var Hn={discard:function(n){return c(n)}},Vr=function(n){return function(t){return function(r){return function(e){return c(n)(t)(function(o){return o?r:e})}}}};var Se=function(n){return function(t){return n===t}},Oa=Se,ka=Se;var Ba=Se,za=Se;var jr={eq:za};var Gr={eq:ka},Va={eq:Ba},qt={eq:Oa};var mn=function(n){return n.eq};var We=function(n){return function(t){return function(r){return mn(qt)(mn(n)(t)(r))(!1)}}};var ja=function(n){return function(){return n}},Ga=function(n){return function(t){return function(){return t(n())()}}};var yt=function(n){return function(t){return function(r){return c(n.Bind1())(t)(function(e){return c(n.Bind1())(r)(function(o){return i(n.Applicative0())(e(o))})})}}};var Qa=function(n){return Math.min(Math.abs(n),2147483647)},Ka=function(n){return function(t){return t===0?0:t>0?Math.floor(n/t):-Math.floor(n/-t)}},Ya=function(n){return function(t){if(t===0)return 0;var r=Math.abs(t);return(n%r+r)%r}};var Xa=function(n){return function(t){return n-t|0}};var Za=function(n){return function(t){return n+t|0}},nu=function(n){return function(t){return n*t|0}};var wo={add:Za,zero:0,mul:nu,one:1};var tu={sub:Xa,Semiring0:function(){return wo}};var ru={Ring0:function(){return tu}};var He=function(n){return n.mod};var Pe={degree:Qa,div:Ka,mod:Ya,CommutativeRing0:function(){return ru}},Ue=function(n){return n.div};var nn=function(){function n(){}return n.value=new n,n}(),tn=function(){function n(){}return n.value=new n,n}(),Cn=function(){function n(){}return n.value=new n,n}();var B=function(n){return n.append};var gn=function(n){return n.mempty};var uu=function(n,t,r){var e=0,o;return function(a){if(e===2)return o;if(e===1)throw new ReferenceError(n+" was needed before it finished initializing (module "+t+", line "+a+")",t,a);return e=1,o=r(),e=2,o}},Nt={Applicative0:function(){return At},Bind1:function(){return $n}},$n={bind:Ga,Apply0:function(){return fu(0)}},At={pure:ja,Apply0:function(){return fu(0)}},iu=uu("functorEffect","Effect",function(){return{map:zr(At)}}),fu=uu("applyEffect","Effect",function(){return{apply:yt(Nt),Functor0:function(){return iu(0)}}}),q=iu(20);var $e={liftEffect:Z(an),Monad0:function(){return Nt}},I=function(n){return n.liftEffect};function cu(n){return function(t){return function(){return setTimeout(t,n)}}}function lu(n){return function(){clearTimeout(n)}}function su(n){return function(t){return function(){return setInterval(t,n)}}}function pu(n){return function(){clearInterval(n)}}var _u=function(n){return function(t){return function(r){return function(e){return function(o){return e<o?n:e===o?t:r}}}}};var mu=_u;var du=_u;var Rt=function(){return{compare:mu(nn.value)(Cn.value)(tn.value),Eq0:function(){return Gr}}}(),Je=function(){return{compare:du(nn.value)(Cn.value)(tn.value),Eq0:function(){return Va}}}();var qe=cu,vu=su;var Du=lu,Cu=pu;var gu=String.fromCharCode(65535),yu=String.fromCharCode(0),Yf=Number.POSITIVE_INFINITY,Xf=Number.NEGATIVE_INFINITY;var _t=function(n){return n.top};var Oe={top:2147483647,bottom:-2147483648,Ord0:function(){return Rt}},ir={top:gu,bottom:yu,Ord0:function(){return Je}};var mt=function(n){return n.bottom};var bu=function(n){return n.toString()};var Ro={show:bu};var X=function(n){return n.show};var b=function(){function n(){}return n.value=new n,n}(),D=function(){function n(t){this.value0=t}return n.create=function(t){return new n(t)},n}();var zt=function(n){return function(t){return function(r){if(r instanceof b)return n;if(r instanceof D)return t(r.value0);throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): "+[n.constructor.name,t.constructor.name,r.constructor.name])}}},xr=zt(!0)(j(!1));var ct={map:function(n){return function(t){return t instanceof D?new D(n(t.value0)):b.value}}};var fr=function(){return function(n){if(n instanceof D)return n.value0;throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): "+[n.constructor.name])}};var Fn=function(){function n(t){this.value0=t}return n.create=function(t){return new n(t)},n}(),wn=function(){function n(t){this.value0=t}return n.create=function(t){return new n(t)},n}();var Qn=function(n){return n.ask};var cr={map:function(n){return function(t){return n(t)}}};var Mu=function(n){return function(){return{value:n}}};var Tt=function(n){return function(){return n.value}};var jt=function(n){return function(t){return function(){t.value=n}}};var sr=Mu;var On=function(){function n(t){this.value0=t}return n.create=function(t){return new n(t)},n}(),Pn=function(){function n(t){this.value0=t}return n.create=function(t){return new n(t)},n}(),dt=function(n){return n.tailRecM};var Ir={tailRecM:function(n){return function(t){var r=function(e){if(e instanceof Pn)return e.value0;throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 113, column 30 - line 113, column 44): "+[e.constructor.name])};return function(){var o=Nn($n)(sr)(n(t))();return function(){for(;!function(){var v=Tt(o)();if(v instanceof On){var F=n(v.value0)();return jt(F)(o)(),!1}if(v instanceof Pn)return!0;throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 104, column 22 - line 109, column 28): "+[v.constructor.name])}(););return{}}(),u(q)(r)(Tt(o))()}}},Monad0:function(){return Nt}};var Q=function(){function n(t,r){this.value0=t,this.value1=r}return n.create=function(t){return function(r){return new n(t,r)}},n}();var rt=function(n){return n.value1};var lt=function(n){return n.value0};var Mn=function(n){return n.lift};var f=function(n){return n};var Iu=function(){return f};var _r=Iu;var Ge=function(n){return n};var te=function(n){return n};var Lu={lift:function(n){return function(t){return Ge(j(t))}}},Nu=function(n){return function(t){return function(r){return n(t(r))}}},re=function(n){return{map:function(){var t=u(n);return function(r){return Nu(t(r))}}()}};var Qe=function(n){return{apply:function(t){return function(r){return function(e){return _n(n)(t(e))(r(e))}}},Functor0:function(){return re(n.Functor0())}}},ee=function(n){return{bind:function(t){return function(r){return function(e){return c(n)(t(e))(function(o){var a=r(o);return a(e)})}}},Apply0:function(){return Qe(n.Apply0())}}};var Nr=function(n){return{pure:function(){var t=i(n);return function(r){return Ge(j(t(r)))}}(),Apply0:function(){return Qe(n.Apply0())}}},dr=function(n){return{Applicative0:function(){return Nr(n.Applicative0())},Bind1:function(){return ee(n.Bind1())}}},Ke=function(n){return{ask:i(n.Applicative0()),Monad0:function(){return dr(n)}}};var oe=function(n){return{liftEffect:function(){var t=Mn(Lu)(n.Monad0()),r=I(n);return function(e){return t(r(e))}}(),Monad0:function(){return dr(n.Monad0())}}},Ye=function(n){return{tailRecM:function(t){return function(r){var e=function(o){return function(a){var v=t(a);return Nn(n.Monad0().Bind1())(i(n.Monad0().Applicative0()))(v(o))}};return function(o){return dt(n)(e(o))(r)}}},Monad0:function(){return dr(n.Monad0())}}};var ae=function(n){return n.bimap};var Ze={bimap:function(n){return function(t){return function(r){if(r instanceof Fn)return new Fn(n(r.value0));if(r instanceof wn)return new wn(t(r.value0));throw new Error("Failed pattern match at Data.Bifunctor (line 32, column 1 - line 34, column 36): "+[n.constructor.name,t.constructor.name,r.constructor.name])}}}};var ue=f,Au=f;var Wu=function(n){return function(t){return function(r){for(var e=t,o=r.length,a=o-1;a>=0;a--)e=n(r[a])(e);return e}}},Hu=function(n){return function(t){return function(r){for(var e=t,o=r.length,a=0;a<o;a++)e=n(e)(r[a]);return e}}};var Qt=function(n){return n.foldr};var le=function(n){return function(t){return function(r){return Qt(t)(function(){var e=Re(n.Apply0());return function(o){return e(r(o))}}())(i(n)(void 0))}}};var Vu=function(n){return function(t){return function(r){return Qt(n)(function(e){return function(o){return B(t.Semigroup0())(r(e))(o)}})(gn(t))}}},Yn={foldr:Wu,foldl:Hu,foldMap:function(n){return Vu(Yn)(n)}};var ju=function(){function n(o){return[o]}function t(o){return function(a){return[o,a]}}function r(o){return function(a){return function(v){return[o,a,v]}}}function e(o){return function(a){return o.concat(a)}}return function(o){return function(a){return function(v){return function(F){return function(R){function on(z,In){switch(In-z){case 0:return v([]);case 1:return a(n)(F(R[z]));case 2:return o(a(t)(F(R[z])))(F(R[z+1]));case 3:return o(o(a(r)(F(R[z])))(F(R[z+1])))(F(R[z+2]));default:var An=z+Math.floor((In-z)/4)*2;return o(a(e)(on(z,An)))(on(An,In))}}return on(0,R.length)}}}}}}();var vr=function(n){return n.traverse};var bl=function(n){return function(t){return vr(n)(t)(Z(an))}},Rr={traverse:function(n){return ju(_n(n.Apply0()))(u(n.Apply0().Functor0()))(i(n))},sequence:function(n){return bl(Rr)(n)},Functor0:function(){return Mt},Foldable1:function(){return Yn}},Sr=function(n){return n.sequence};var Dr=function(){var n={},t="Pure",r="Throw",e="Catch",o="Sync",a="Async",v="Bind",F="Bracket",R="Fork",on="Sequential",z="Map",In="Apply",An="Alt",ln="Cons",Ut="Resume",To="Release",Ua="Finalizer",Eo="Finalized",$a="Forked",Im="Fiber",wm="Thunk";function h(d,U,dn,V){this.tag=d,this._1=U,this._2=dn,this._3=V}function at(d){var U=function(dn,V,C){return new h(d,dn,V,C)};return U.tag=d,U}function ho(d){return new h(t,void 0)}function Tf(d){try{d()}catch(U){setTimeout(function(){throw U},0)}}function Ef(d,U,dn){try{return U(dn())}catch(V){return d(V)}}function hf(d,U,dn){try{return U(dn)()}catch(V){return dn(d(V))(),ho}}var Me=function(){var d=1024,U=0,dn=0,V=new Array(d),C=!1;function l(){var k;for(C=!0;U!==0;)U--,k=V[dn],V[dn]=void 0,dn=(dn+1)%d,k();C=!1}return{isDraining:function(){return C},enqueue:function(k){var x,pn;U===d&&(pn=C,l(),C=pn),V[(dn+U)%d]=k,U++,C||l()}}}();function Ff(d){var U={},dn=0,V=0;return{register:function(C){var l=dn++;C.onComplete({rethrow:!0,handler:function(k){return function(){V--,delete U[l]}}})(),U[l]=C,V++},isEmpty:function(){return V===0},killAll:function(C,l){return function(){if(V===0)return l();var k=0,x={};function pn(K){x[K]=U[K].kill(C,function(Tn){return function(){delete x[K],k--,d.isLeft(Tn)&&d.fromLeft(Tn)&&setTimeout(function(){throw d.fromLeft(Tn)},0),k===0&&l()}})()}for(var Rn in U)U.hasOwnProperty(Rn)&&(k++,pn(Rn));return U={},dn=0,V=0,function(K){return new h(o,function(){for(var Tn in x)x.hasOwnProperty(Tn)&&x[Tn]()})}}}}}var Er=0,Vn=1,xe=2,Ie=3,we=4,Zn=5,kr=6;function Fo(d,U,dn){var V=0,C=Er,l=dn,k=null,x=null,pn=null,Rn=null,K=null,Tn=0,er=0,ut=null,$t=!0;function Jt(g){for(var y,S,W;;)switch(y=null,S=null,W=null,C){case xe:C=Vn;try{l=pn(l),Rn===null?pn=null:(pn=Rn._1,Rn=Rn._2)}catch(Sn){C=Zn,k=d.left(Sn),l=null}break;case Ie:d.isLeft(l)?(C=Zn,k=l,l=null):pn===null?C=Zn:(C=xe,l=d.fromRight(l));break;case Vn:switch(l.tag){case v:pn&&(Rn=new h(ln,pn,Rn)),pn=l._2,C=Vn,l=l._1;break;case t:pn===null?(C=Zn,l=d.right(l._1)):(C=xe,l=l._1);break;case o:C=Ie,l=Ef(d.left,d.right,l._1);break;case a:C=we,l=hf(d.left,l._1,function(Sn){return function(){V===g&&(V++,Me.enqueue(function(){V===g+1&&(C=Ie,l=Sn,Jt(V))}))}});return;case r:C=Zn,k=d.left(l._1),l=null;break;case e:pn===null?K=new h(ln,l,K,x):K=new h(ln,l,new h(ln,new h(Ut,pn,Rn),K,x),x),pn=null,Rn=null,C=Vn,l=l._1;break;case F:Tn++,pn===null?K=new h(ln,l,K,x):K=new h(ln,l,new h(ln,new h(Ut,pn,Rn),K,x),x),pn=null,Rn=null,C=Vn,l=l._1;break;case R:C=Ie,y=Fo(d,U,l._2),U&&U.register(y),l._1&&y.run(),l=d.right(y);break;case on:C=Vn,l=xf(d,U,l._1);break}break;case Zn:if(pn=null,Rn=null,K===null)C=kr,l=x||k||l;else switch(y=K._3,W=K._1,K=K._2,W.tag){case e:x&&x!==y&&Tn===0?C=Zn:k&&(C=Vn,l=W._2(d.fromLeft(k)),k=null);break;case Ut:x&&x!==y&&Tn===0||k?C=Zn:(pn=W._1,Rn=W._2,C=xe,l=d.fromRight(l));break;case F:Tn--,k===null&&(S=d.fromRight(l),K=new h(ln,new h(To,W._2,S),K,y),(x===y||Tn>0)&&(C=Vn,l=W._3(S)));break;case To:K=new h(ln,new h(Eo,l,k),K,x),C=Vn,x&&x!==y&&Tn===0?l=W._1.killed(d.fromLeft(x))(W._2):k?l=W._1.failed(d.fromLeft(k))(W._2):l=W._1.completed(d.fromRight(l))(W._2),k=null,Tn++;break;case Ua:Tn++,K=new h(ln,new h(Eo,l,k),K,x),C=Vn,l=W._1;break;case Eo:Tn--,C=Zn,l=W._1,k=W._2;break}break;case kr:for(var Dn in ut)ut.hasOwnProperty(Dn)&&($t=$t&&ut[Dn].rethrow,Tf(ut[Dn].handler(l)));ut=null,x&&k?setTimeout(function(){throw d.fromLeft(k)},0):d.isLeft(l)&&$t&&setTimeout(function(){if($t)throw d.fromLeft(l)},0);return;case Er:C=Vn;break;case we:return}}function vn(g){return function(){if(C===kr)return $t=$t&&g.rethrow,g.handler(l)(),function(){};var y=er++;return ut=ut||{},ut[y]=g,function(){ut!==null&&delete ut[y]}}}function T(g,y){return function(){if(C===kr)return y(d.right(void 0))(),function(){};var S=vn({rethrow:!1,handler:function(){return y(d.right(void 0))}})();switch(C){case Er:x=d.left(g),C=kr,l=x,Jt(V);break;case we:x===null&&(x=d.left(g)),Tn===0&&(C===we&&(K=new h(ln,new h(Ua,l(g)),K,x)),C=Zn,l=null,k=null,Jt(++V));break;default:x===null&&(x=d.left(g)),Tn===0&&(C=Zn,l=null,k=null)}return S}}function w(g){return function(){var y=vn({rethrow:!1,handler:g})();return C===Er&&Jt(V),y}}return{kill:T,join:w,onComplete:vn,isSuspended:function(){return C===Er},run:function(){C===Er&&(Me.isDraining()?Jt(V):Me.enqueue(function(){Jt(V)}))}}}function Mf(d,U,dn,V){var C=0,l={},k=0,x={},pn=new Error("[ParAff] Early exit"),Rn=null,K=n;function Tn(vn,T,w){var g=T,y=null,S=null,W=0,Dn={},Sn,Br;n:for(;;)switch(Sn=null,g.tag){case $a:if(g._3===n&&(Sn=l[g._1],Dn[W++]=Sn.kill(vn,function(If){return function(){W--,W===0&&w(If)()}})),y===null)break n;g=y._2,S===null?y=null:(y=S._1,S=S._2);break;case z:g=g._2;break;case In:case An:y&&(S=new h(ln,y,S)),y=g,g=g._1;break}if(W===0)w(d.right(void 0))();else for(Br=0,Sn=W;Br<Sn;Br++)Dn[Br]=Dn[Br]();return Dn}function er(vn,T,w){var g,y,S,W,Dn,Sn;d.isLeft(vn)?(g=vn,y=null):(y=vn,g=null);n:for(;;){if(S=null,W=null,Dn=null,Sn=null,Rn!==null)return;if(T===null){V(g||y)();return}if(T._3!==n)return;switch(T.tag){case z:g===null?(T._3=d.right(T._1(d.fromRight(y))),y=T._3):T._3=g;break;case In:if(S=T._1._3,W=T._2._3,g){if(T._3=g,Dn=!0,Sn=k++,x[Sn]=Tn(pn,g===S?T._2:T._1,function(){return function(){delete x[Sn],Dn?Dn=!1:w===null?er(g,null,null):er(g,w._1,w._2)}}),Dn){Dn=!1;return}}else{if(S===n||W===n)return;y=d.right(d.fromRight(S)(d.fromRight(W))),T._3=y}break;case An:if(S=T._1._3,W=T._2._3,S===n&&d.isLeft(W)||W===n&&d.isLeft(S))return;if(S!==n&&d.isLeft(S)&&W!==n&&d.isLeft(W))g=y===S?W:S,y=null,T._3=g;else if(T._3=y,Dn=!0,Sn=k++,x[Sn]=Tn(pn,y===S?T._2:T._1,function(){return function(){delete x[Sn],Dn?Dn=!1:w===null?er(y,null,null):er(y,w._1,w._2)}}),Dn){Dn=!1;return}break}w===null?T=null:(T=w._1,w=w._2)}}function ut(vn){return function(T){return function(){delete l[vn._1],vn._3=T,er(T,vn._2._1,vn._2._2)}}}function $t(){var vn=Vn,T=dn,w=null,g=null,y,S;n:for(;;)switch(y=null,S=null,vn){case Vn:switch(T.tag){case z:w&&(g=new h(ln,w,g)),w=new h(z,T._1,n,n),T=T._2;break;case In:w&&(g=new h(ln,w,g)),w=new h(In,n,T._2,n),T=T._1;break;case An:w&&(g=new h(ln,w,g)),w=new h(An,n,T._2,n),T=T._1;break;default:S=C++,vn=Zn,y=T,T=new h($a,S,new h(ln,w,g),n),y=Fo(d,U,y),y.onComplete({rethrow:!1,handler:ut(T)})(),l[S]=y,U&&U.register(y)}break;case Zn:if(w===null)break n;w._1===n?(w._1=T,vn=Vn,T=w._2,w._2=n):(w._2=T,T=w,g===null?w=null:(w=g._1,g=g._2))}for(K=T,S=0;S<C;S++)l[S].run()}function Jt(vn,T){Rn=d.left(vn);var w;for(var g in x)if(x.hasOwnProperty(g)){w=x[g];for(g in w)w.hasOwnProperty(g)&&w[g]()}x=null;var y=Tn(vn,K,T);return function(S){return new h(a,function(W){return function(){for(var Dn in y)y.hasOwnProperty(Dn)&&y[Dn]();return ho}})}}return $t(),function(vn){return new h(a,function(T){return function(){return Jt(vn,T)}})}}function xf(d,U,dn){return new h(a,function(V){return function(){return Mf(d,U,dn,V)}})}return h.EMPTY=n,h.Pure=at(t),h.Throw=at(r),h.Catch=at(e),h.Sync=at(o),h.Async=at(a),h.Bind=at(v),h.Bracket=at(F),h.Fork=at(R),h.Seq=at(on),h.ParMap=at(z),h.ParApply=at(In),h.ParAlt=at(An),h.Fiber=Fo,h.Supervisor=Ff,h.Scheduler=Me,h.nonCanceler=ho,h}(),xl=Dr.Pure,Il=Dr.Throw;var wl=Dr.Sync;var fi=Dr.Async;var Ll=function(){function n(r,e){return r===0&&typeof setImmediate<"u"?setImmediate(e):setTimeout(e,r)}function t(r,e){return r===0&&typeof clearImmediate<"u"?clearImmediate(e):clearTimeout(e)}return function(r,e){return Dr.Async(function(o){return function(){var a=n(e,o(r()));return function(){return Dr.Sync(function(){return r(t(e,a))})}}})}}(),Nl=Dr.Seq;var Ql=function(){function n(t,r){this.value0=t,this.value1=r}return n.create=function(t){return function(r){return new n(t,r)}},n}(),Wr=function(){function n(t){this.value0=t}return n.create=function(t){return new n(t)},n}(),me=function(){function n(t){this.value0=t}return n.create=function(t){return new n(t)},n}(),si=function(n){return{lift:function(t){return function(r){return new Wr(function(e){return u(t.Bind1().Apply0().Functor0())(Fn.create)(r)})}}}};var io=function(n){return function(t){return new me(Au(new Ql(n,t)))}},Yo=function(n){return function(t){return{map:function(r){return function(e){if(e instanceof Wr)return new Wr(function(o){return u(t)(ae(Ze)(r)(u(n)(u(Yo(n)(t))(r))))(e.value0(void 0))});if(e instanceof me)return ue(function(o){return io(o.value0)(function(){var a=u(Yo(n)(t))(r);return function(v){return a(o.value1(v))}}())})(e.value0);throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 64, column 1 - line 66, column 71): "+[r.constructor.name,e.constructor.name])}}}}};var pi=function(n){return function(t){return{Applicative0:function(){return mi(n)(t)},Bind1:function(){return _e(n)(t)}}}},_e=function(n){return function(t){return{bind:function(r){return function(e){return r instanceof me?ue(function(o){return io(o.value0)(function(a){return io(function(v){return o.value1(a)})(e)})})(r.value0):io(function(o){return r})(e)}},Apply0:function(){return _i(n)(t)}}}},_i=function(n){return function(t){return{apply:yt(pi(n)(t)),Functor0:function(){return Yo(n)(t.Bind1().Apply0().Functor0())}}}},mi=function(n){return function(t){return{pure:function(r){return new Wr(function(e){return i(t.Applicative0())(new Fn(r))})},Apply0:function(){return _i(n)(t)}}}};var Kl=function(n){return function(t){var r=function(e){if(e instanceof Wr)return u(t.Monad0().Bind1().Apply0().Functor0())(Pn.create)(e.value0(void 0));if(e instanceof me)return ue(function(o){var a=o.value0(void 0);if(a instanceof Wr)return c(t.Monad0().Bind1())(a.value0(void 0))(function(v){if(v instanceof Fn)return i(t.Monad0().Applicative0())(new On(o.value1(v.value0)));if(v instanceof wn)return i(t.Monad0().Applicative0())(new Pn(new wn(u(n)(function(F){return c(_e(n)(t.Monad0()))(F)(o.value1)})(v.value0))));throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 57, column 22 - line 59, column 69): "+[v.constructor.name])});if(a instanceof me)return ue(function(v){return i(t.Monad0().Applicative0())(new On(c(_e(n)(t.Monad0()))(v.value0(void 0))(function(F){return c(_e(n)(t.Monad0()))(v.value1(F))(o.value1)})))})(a.value0);throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 55, column 7 - line 62, column 60): "+[a.constructor.name])})(e.value0);throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 51, column 3 - line 51, column 75): "+[e.constructor.name])};return dt(t)(r)}},di=function(n){return function(t){return function(r){var e=function(o){if(o instanceof Fn)return i(t.Monad0().Applicative0())(new Pn(o.value0));if(o instanceof wn)return u(t.Monad0().Bind1().Apply0().Functor0())(On.create)(r(o.value0));throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 144, column 3 - line 144, column 63): "+[o.constructor.name])};return dt(t)(Io(t.Monad0().Bind1())(e)(Kl(n)(t)))}}};var vi=function(n){return function(t){return{tailRecM:function(r){var e=function(o){return c(_e(n)(t))(r(o))(function(a){if(a instanceof On)return e(a.value0);if(a instanceof Pn)return i(mi(n)(t))(a.value0);throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 87, column 15 - line 89, column 25): "+[a.constructor.name])})};return e},Monad0:function(){return pi(n)(t)}}}};var yi=function(n){return function(t){return function(r){return function(e){return function(o){return function(a){for(var v=[],F=a;;){var R=o(F);if(n(R))return v;var on=t(R);v.push(r(on)),F=e(on)}}}}}}};var bi=function(n){return function(t){return function(r){return function(e){return function(o){return function(a){for(var v=[],F=a;;){var R=o(F);v.push(r(R));var on=e(R);if(n(on))return v;F=t(on)}}}}}}};var Ei={unfoldr1:bi(xr)(fr())(lt)(rt)};var de=function(n){return n.unfoldr};var na={unfoldr:yi(xr)(fr())(lt)(rt),Unfoldable10:function(){return Ei}};var Ds=function(n){return function(t){var r=function(e){return di(cr)(e)(function(){var o=i(e.Monad0().Applicative0()),a=_r();return function(v){return o(a(v))}}())};return r(n)(t(vi(cr)(n.Monad0()))(Mn(si(cr))(n.Monad0()))(r(n)))}},Cs=function(n){return function(t){return function(r){return function(e){return Ds(t)(function(o){return function(a){return function(v){return le(o.Monad0().Applicative0())(n)(function(F){return a(r(F))})(e)}}})}}}};var Kt=function(n){return function(t){return Bn(Cs(n)(t))}};var ra=n=>()=>{let t={callbacks:new Set,dependencies:new Set};return t.effect=n(t),t},ea=()=>new Set,oa=n=>t=>()=>{n.dependencies.add(t),t.add(n)},Fi=n=>t=>()=>{n.dependencies.delete(t),t.delete(n)},co=n=>()=>{n.dependencies.forEach(t=>{Fi(n)(t)()})},aa=n=>t=>()=>{n.callbacks.add(t)},ua=n=>()=>{n.callbacks.clear()},ia=n=>()=>[...n.callbacks],fa=n=>n.effect,ca=n=>()=>[...n];var la=function(n){return function(){var r=ia(n)();return Kt(Yn)(Ir)(r)(function(e){return i(At)(void 0)})(),ua(n)()}};var xi=Ye(Ir),Ii=dr(Nt),Ln=oe($e),wi=Ke(Nt),Un=re(q),yn=ee($n);var N=Nr(At);var Mi=function(n){return function(t){return te(t)(new D(n))}},lo=function(n){return function(t){return function(r){return I(n)(function(){var o=sr(r)(),a=ea(),v=function(R){return I(Ln)(function(){var z=Tt(o)(),In=R(z),An=We(t)(z)(In);if(An){var ln=ca(a)();return Kt(Yn)(Ir)(ln)(function(Ut){return function(){return co(Ut)(),la(Ut)()}})(),jt(In)(o)(),Kt(Yn)(Ir)(ln)(function(Ut){return fa(Ut)})()}return void 0})},F=c(yn)(Qn(wi))(function(R){return Wn(Hn)(yn)(I(Ln)(function(){if(R instanceof D)return oa(R.value0)(a);if(R instanceof b)return i(At)(void 0);throw new Error("Failed pattern match at Jelly.Data.Jelly (line 55, column 18 - line 57, column 29): "+[R.constructor.name])}()))(function(){return I(Ln)(Tt(o))})});return new Q(F,v)})}}};var Dt=function(n){return te(n)(b.value)},sa=function(n){return c(yn)(Qn(wi))(function(t){var r=Dt(n);if(t instanceof D)return I(Ln)(aa(t.value0)(r));if(t instanceof b)return i(N)(void 0);throw new Error("Failed pattern match at Jelly.Data.Jelly (line 95, column 3 - line 98, column 25): "+[t.constructor.name])})};var Li=function(n){return c(yn)(I(Ln)(ra(function(t){return Mi(t)(n)})))(function(t){return Wn(Hn)(yn)(I(Ln)(Mi(t)(n)))(function(){var r=function(){return co(t)(),la(t)()};return sa(I(Ln)(r))})})};var bs=function(n){return n},pa=Ye(xi);var Xn=oe(Ln),so=Ke(Ii),Ni=re(Un),$=ee(yn);var Yt=Nr(N),po=function(n){return function(t){return te(t)(n)}},Hr=function(n){return bs(Ge(j(n)))};var _a=function(n){return function(t){return t.join(n)}};var ma=function(){function n(t,r){this.value0=t,this.value1=r}return n.create=function(t){return function(r){return new n(t,r)}},n}(),da=function(){function n(t,r){this.value0=t,this.value1=r}return n.create=function(t){return function(r){return new n(t,r)}},n}(),De=function(){return da.create}(),hs=function(){return ma.create}(),bn=function(n){return hs("class")(u(Un)(_a(" "))(Sr(Rr)(N)(n)))};var Pt=function(n){return lo(Xn)(n)};var mo=function(n){return Hr(Li(n))};var Cr=function(n){return function(t){return function(){return t[n]}}},Ms=Cr("URL"),xs=Cr("documentURI"),Is=Cr("origin"),ws=Cr("compatMode"),Ls=Cr("characterSet"),Ns=Cr("contentType");var As=Cr("documentElement");function va(n){return function(t){return function(){return t.createElement(n)}}}function Da(n){return function(t){return function(){return t.createTextNode(n)}}}function Ri(n,t,r){return n==null?t:r(n)}var hn=function(n){return Ri(n,b.value,D.create)};var vo=function(n){return function(t){return t[n]}},$s=vo("namespaceURI"),Js=vo("prefix"),qs=vo("localName"),Os=vo("tagName");function Ca(n){return function(t){return function(r){return function(){r.setAttribute(n,t)}}}}var Do=function(n){return function(t){return function(){return t[n]}}},Bs=Do("children"),zs=Do("firstElementChild"),Vs=Do("lastElementChild"),js=Do("childElementCount");var ga=f,Hi=f;var Ct=function(n){return function(t){return function(){return t[n]}}};var Xs=Ct("baseURI"),Zs=Ct("ownerDocument"),np=Ct("parentNode"),tp=Ct("parentElement");var rp=Ct("childNodes"),ep=Ct("firstChild"),op=Ct("lastChild"),ap=Ct("previousSibling"),up=Ct("nextSibling"),ip=Ct("nodeValue");var fp=Ct("textContent");function ya(n){return function(t){return function(){t.textContent=n}}}function ba(n){return function(t){return function(r){return function(){r.insertBefore(n,t)}}}}function Ce(n){return function(t){return function(){t.appendChild(n)}}}function Ta(n){return function(t){return function(){t.removeChild(n)}}}function Ur(n){return n.charCodeAt(0)}function Pi(n){return String.fromCharCode(n)}var ge=function(n){return n.toEnum};var gr=function(n){return n.fromEnum},Ui=function(n){return function(t){return function(r){return function(e){var o=ge(n)(e);if(o instanceof D)return o.value0;if(o instanceof b){var a=e<gr(n)(mt(n.Bounded0()));return a?t:r}throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): "+[o.constructor.name])}}}};var $r=function(n){return function(t){return function(r){return n(t(r)+1|0)}}},Jr=function(n){return function(t){return function(r){return n(t(r)-1|0)}}};var Ea=function(n){return n>=mt(Oe)&&n<=_t(Oe)?new D(Pi(n)):b.value},pp={succ:$r(Ea)(Ur),pred:Jr(Ea)(Ur),Ord0:function(){return Je}};var yr=function(){return{cardinality:Ur(_t(ir))-Ur(mt(ir))|0,toEnum:Ea,fromEnum:Ur,Bounded0:function(){return ir},Enum1:function(){return pp}}}();var Ji=f;function ha(n){return function(){return function(t){return n(t)()}}}function Fa(n){return function(t){return function(r){return function(e){return function(){return e.addEventListener(n,t,r)}}}}}var Zt=function(){return window};function qi(n){return function(){return n.body}}var Ma=f;var Oi=function(){var n=u(q)(hn);return function(t){return n(qi(t))}}();var Bi=f;function xa(n){return function(t){return function(){t.href=n}}}function qr(n){return function(){return n.document}}function Ia(n){return function(){return n.location}}var nr=function(n){return c($)(I(Xn)(u(q)(Ji)(Nn($n)(function(){var t=Da("");return function(r){return t(Ma(r))}}())(Nn($n)(qr)(Zt)))))(function(t){return Wn(Hn)($)(mo(c(yn)(n)(function(r){return Wn(Hn)(yn)(I(Ln)(ya(r)(t)))(function(){return i(N)(void 0)})})))(function(){return i(Yt)(t)})})},v_=function(n){return function(t){if(t instanceof ma)return mo(c(yn)(t.value1)(function(r){return I(Ln)(Ca(t.value0)(r)(n))}));if(t instanceof da)return c($)(I(Xn)(ha(function(r){return Dt(t.value1(r))})))(function(r){return I(Xn)(Fa(t.value0)(r)(!1)(Hi(n)))});throw new Error("Failed pattern match at Jelly.HTML (line 30, column 3 - line 40, column 30): "+[t.constructor.name])}};var D_=nr(i(N)("")),Na=function(n){return function(t){return c($)(Hr(n))(function(r){return r?t:D_})}},C_=function(n){return function(t){return c($)(I(Xn)(sr(b.value)))(function(r){return c($)(Qn(so))(function(e){return mo(c(yn)(I(Ln)(Tt(r)))(function(o){return c(yn)(po(e)(t))(function(a){return Wn(Hn)(yn)(I(Ln)(function(){if(o instanceof b)return Ce(a)(n);if(o instanceof D)return function(){return ba(a)(o.value0)(n)(),Ta(o.value0)(n)()};throw new Error("Failed pattern match at Jelly.HTML (line 53, column 16 - line 58, column 39): "+[o.constructor.name])}()))(function(){return I(Ln)(jt(new D(a))(r))})})}))})})}},ye=function(n){return function(t){return function(r){return c($)(I(Xn)(Nn($n)(function(){var e=va(n);return function(o){return e(Ma(o))}}())(Nn($n)(qr)(Zt))))(function(e){return Wn(Hn)($)(Kt(Yn)(pa)(t)(v_(e)))(function(){return Wn(Hn)($)(Kt(Yn)(pa)(r)(C_(ga(e))))(function(){return i(Yt)(ga(e))})})})}}};var br=ye("button"),kn=ye("div");var Te=function(n){return c($)(Pt(qt)(!1))(function(t){return c($)(I(Xn)(qe(100)(Dt(t.value1(function(r){return!0})))))(function(){return kn([bn([Vr(yn)(t.value0)(i(N)("scale-100 opacity-100"))(i(N)("scale-50 opacity-0")),i(N)("transition-all")])])(n)})})};var Qi=Qn(so);var tr=function(){function n(){}return n.value=new n,n}(),Tr=function(){function n(){}return n.value=new n,n}(),Ki={eq:function(n){return function(t){return n instanceof tr&&t instanceof tr||n instanceof Tr&&t instanceof Tr}}},Aa=c($)(Qi)(function(n){return i(Yt)(n.colorMode)}),y_=function(n){if(n instanceof tr)return{text:{primary:"text-slate-900",highlight:"text-pink-600",reverse:"text-white"},background:{primary:"bg-white",highlight:"bg-pink-600",reverse:"bg-slate-900"}};if(n instanceof Tr)return{text:{primary:"text-white",highlight:"text-pink-600",reverse:"text-slate-900"},background:{primary:"bg-slate-900",highlight:"bg-pink-600",reverse:"bg-white"}};throw new Error("Failed pattern match at Contexts.ColorMode (line 37, column 18 - line 61, column 6): "+[n.constructor.name])},Or=c($)(Aa)(function(n){return i(Yt)(u(Un)(y_)(n.value0))});var Yi=c($)(Or)(function(n){return c($)(Pt(Gr)(0))(function(t){return Te([kn([bn([i(N)("h-24 w-24 flex flex-col justify-center items-center relative")])])([br([bn([i(N)("h-24 w-24 -rotate-[20deg] hover:rotate-0 transition-all absolute origin-center rounded-md"),Gn(Un)(n)(function(r){return r.background.reverse})]),De("click")(function(r){return t.value1(function(e){return e+1|0})})])([]),kn([bn([i(N)("flex justify-center items-center text-3xl z-20 relative pointer-events-none transition-colors"),Gn(Un)(n)(function(r){return r.text.reverse})])])([nr(u(Un)(X(Ro))(t.value0))])])])})});var Ee=function(n){return ye("i")([bn([n])])([])};var Xi=c($)(Or)(function(n){return kn([bn([i(N)("relative w-56 mt-6 h-5 z-10  flex flex-row justify-center items-end rounded-t-md transition-colors"),Gn(Un)(n)(function(t){return t.background.highlight})])])([kn([bn([i(N)("relative text-5xl z-20 font-AlfaSlabOne font-extrabold transition-colors"),Gn(Un)(n)(function(t){return t.text.primary})])])([nr(i(N)("JELLY"))])])});var h_=typeof Array.from=="function",F_=typeof Symbol<"u"&&Symbol!=null&&typeof Symbol.iterator<"u"&&typeof String.prototype[Symbol.iterator]=="function",M_=typeof String.prototype.fromCodePoint=="function",x_=typeof String.prototype.codePointAt=="function",Zi=function(n){return x_?function(t){return t.codePointAt(0)}:n};var nf=function(n){return M_?String.fromCodePoint:n},tf=function(n){return function(t){return F_?function(r){for(var e="",o=r[Symbol.iterator](),a=0;a<t;++a){var v=o.next();if(v.done)return e;e+=v.value}return e}:n(t)}},rf=function(n){return function(t){return h_?function(r){return Array.from(r,t)}:n}};var w_=function(n){return function(t){if(n<1)return[];var r=new Array(n);return r.fill(t)}},L_=function(n){return function(t){for(var r=[],e=0,o=0;o<n;o++)r[e++]=t;return r}},N_=typeof Array.prototype.fill=="function"?w_:L_,A_=function(){function n(o,a){this.head=o,this.tail=a}var t={};function r(o){return function(a){return new n(o,a)}}function e(o){for(var a=[],v=0,F=o;F!==t;)a[v++]=F.head,F=F.tail;return a}return function(o){return function(a){return e(o(r)(t)(a))}}}(),go=function(n){return n.length};var R_=function(){function n(t,r,e,o,a,v){var F,R,on,z,In,An,ln;for(F=a+(v-a>>1),F-a>1&&n(t,r,o,e,a,F),v-F>1&&n(t,r,o,e,F,v),R=a,on=F,z=a;R<F&&on<v;)In=o[R],An=o[on],ln=r(t(In)(An)),ln>0?(e[z++]=An,++on):(e[z++]=In,++R);for(;R<F;)e[z++]=o[R++];for(;on<v;)e[z++]=o[on++]}return function(t){return function(r){return function(e){var o;return e.length<2?e:(o=e.slice(0),n(t,r,o,e.slice(0),0,e.length),o)}}}}();var $_=function(){function n(t,r,e,o,a,v){var F,R,on,z,In,An,ln;for(F=a+(v-a>>1),F-a>1&&n(t,r,o,e,a,F),v-F>1&&n(t,r,o,e,F,v),R=a,on=F,z=a;R<F&&on<v;)In=o[R],An=o[on],ln=r(t(In)(An)),ln>0?(e[z++]=An,++on):(e[z++]=In,++R);for(;R<F;)e[z++]=o[R++];for(;on<v;)e[z++]=o[on++]}return function(t){return function(r){return function(e){return function(){return e.length<2||n(t,r,e,e.slice(0),0,e.length),e}}}}}();var Wa=function(n){return n};var he=function(n){return n.length};var Fe=function(n){return function(t){return t.substring(n)}};var rr=function(n){return function(t){if(n>=0&&n<t.length)return t.charAt(n);throw new Error("Data.String.Unsafe.charAt: Invalid index.")}};var _f=function(n){return function(t){return(((n-55296|0)*1024|0)+(t-56320|0)|0)+65536|0}};var mf=function(n){return 56320<=n&&n<=57343},df=function(n){return 55296<=n&&n<=56319},vf=function(n){var t=he(n);if(t===0)return b.value;if(t===1)return new D({head:gr(yr)(rr(0)(n)),tail:""});var r=gr(yr)(rr(1)(n)),e=gr(yr)(rr(0)(n)),o=df(e)&&mf(r);return o?new D({head:_f(e)(r),tail:Fe(2)(n)}):new D({head:e,tail:Fe(1)(n)})},Dm=function(n){return u(ct)(function(t){return new Q(t.head,t.tail)})(vf(n))},Cm=function(n){return de(na)(Dm)(n)},gm=function(n){var t=gr(yr)(rr(0)(n)),r=df(t)&&he(n)>1;if(r){var e=gr(yr)(rr(1)(n)),o=mf(e);return o?_f(t)(e):t}return t},ym=Zi(gm),bm=rf(Cm)(ym),yo=function(n){return go(bm(n))};var Ha=function(){var n=Ui(yr)(mt(ir))(_t(ir));return function(t){return Wa(n(t))}}(),Tm=function(n){if(n<=65535)return Ha(n);var t=Ue(Pe)(n-65536|0)(1024)+55296|0,r=He(Pe)(n-65536|0)(1024)+56320|0;return Ha(t)+Ha(r)};var Em=nf(Tm),Df=function(n){return function(t){if(n<1)return"";var r=vf(t);return r instanceof D?Em(r.value0.head)+Df(n-1|0)(r.value0.tail):t}},Cf=tf(Df);var bo=function(n){return Hr(sa(n))};var Pa=function(n){return c($)(Pt(jr)("\u25A0"))(function(t){return c($)(Pt(qt)(!1))(function(r){return c($)(I(Xn)(vu(50)(Dt(c(yn)(t.value0)(function(e){return Vr(yn)(r.value0)(t.value1(j(n)))(Wn(Hn)(yn)(t.value1(j(Cf(yo(e))(n)+"\u25A0")))(function(){return ar(N)(yo(e)===yo(n))(r.value1(j(!0)))}))})))))(function(e){return Wn(Hn)($)(bo(I(Ln)(Cu(e))))(function(){return i(Yt)(new Q(t.value0,r.value0))})})})})};var yf=function(n){return function(t){return function(){var e=Nn($n)(Oi)(Nn($n)(qr)(Zt))(),o=Dt(po(n)(t))();if(e instanceof D)return Ce(o)(Bi(e.value0))();if(e instanceof b)return void 0;throw new Error("Failed pattern match at Jelly.RunComponent (line 23, column 3 - line 25, column 25): "+[e.constructor.name])}}};var xm=c($)(Pa("Jelly is a easy way to create interactive web apps."))(function(n){return c($)(Aa)(function(t){return c($)(Or)(function(r){return c($)(Pt(qt)(!1))(function(e){return c($)(I(Xn)(qe(400)(Dt(e.value1(j(!0))))))(function(o){return Wn(Hn)($)(bo(I(Ln)(Du(o))))(function(){return kn([bn([i(N)("h-screen w-screen relative text-lg overflow-hidden flex flex-col items-center font-Inconsolata transition-colors"),Gn(Un)(r)(function(a){return a.background.primary}),Gn(Un)(r)(function(a){return a.text.primary})])])([kn([bn([i(N)("py-3 px-8 flex justify-between w-screen")])])([kn([bn([i(N)("w-12")])])([]),Xi,br([bn([i(N)("w-12 rounded-full hover:scale-110 transition-all flex justify-center items-center")]),De("click")(function(a){return c(yn)(t.value0)(function(v){if(v instanceof tr)return t.value1(Tr.value);if(v instanceof Tr)return t.value1(tr.value);throw new Error("Failed pattern match at Main (line 69, column 17 - line 71, column 45): "+[v.constructor.name])})})])([Ee(c(yn)(t.value0)(function(a){if(a instanceof tr)return i(N)("fa-solid fa-sun fa-lg");if(a instanceof Tr)return i(N)("fa-solid fa-moon fa-lg");throw new Error("Failed pattern match at Main (line 75, column 17 - line 77, column 56): "+[a.constructor.name])}))])]),kn([bn([i(N)("h-1 w-screen mb-10 transition-colors"),Gn(Un)(r)(function(a){return a.background.reverse})])])([]),nr(n.value0),Na(e.value0)(kn([bn([i(N)("flex-grow flex flex-row items-center justify-between w-full px-10")])])([Te([br([bn([i(N)("h-12 w-12 hover:-translate-x-1 transition-transform")])])([Ee(i(N)("fa-xl fa-solid fa-chevron-left"))])]),Yi,Te([br([bn([i(N)("h-12 w-12 hover:translate-x-1 transition-transform")])])([Ee(i(N)("fa-xl fa-solid fa-chevron-right"))])])])),Na(e.value0)(kn([bn([i(N)("flex flex-row w-full justify-between p-10")])])([kn([bn([i(N)("w-12")])])([]),Nn($)(nr)(u(Ni)(lt)(Pa("A Button"))),br([bn([i(N)("w-12 rounded-full hover:scale-110 transition-all flex justify-center items-center")]),De("click")(function(a){return I(Ln)(Nn($n)(xa("https://github.com/yukikurage/purescript-jelly"))(Nn($n)(Ia)(Zt)))})])([Ee(i(N)("fa-xl fa-brands fa-github"))])]))])})})})})})}),bf=function(){var t=lo($e)(Ki)(tr.value)();return yf({colorMode:new Q(t.value0,function(r){return t.value1(j(r))})})(xm)()};bf();})();