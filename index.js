(()=>{var Ne={compose:function(n){return function(t){return function(r){return n(t(r))}}}};var Z=function(n){return n.identity},an={identity:function(n){return n},Semigroupoid0:function(){return Ne}};var Bn=function(n){return function(t){return function(r){return n(r)(t)}}},j=function(n){return function(t){return n}};var qa=function(n){return function(t){for(var r=t.length,e=new Array(r),o=0;o<r;o++)e[o]=n(t[o]);return e}};var u=function(n){return n.map},Zn=function(n){return function(t){return function(r){return u(n)(r)(t)}}};var Mt={map:qa};var mn=function(n){return n.apply};var Se=function(n){return function(t){return function(r){return mn(n)(u(n.Functor0())(j(Z(an)))(t))(r)}}};var i=function(n){return n.pure};var ur=function(n){return function(t){return function(r){if(t)return r;if(!t)return i(n)(void 0);throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): "+[t.constructor.name,r.constructor.name])}}},jr=function(n){return function(t){return function(r){return mn(n.Apply0())(i(n)(t))(r)}}};var Pn=function(n){return n.discard};var c=function(n){return n.bind},Nn=function(n){return Bn(c(n))},wo=function(n){return function(t){return function(r){return function(e){return Nn(n)(t)(r(e))}}}};var Hn={discard:function(n){return c(n)}},Gr=function(n){return function(t){return function(r){return function(e){return c(n)(t)(function(o){return o?r:e})}}}};var We=function(n){return function(t){return n===t}},ka=We,Ba=We;var za=We,Va=We;var Qr={eq:Va};var Kr={eq:Ba},ja={eq:za},Ot={eq:ka};var dn=function(n){return n.eq};var Pe=function(n){return function(t){return function(r){return dn(Ot)(dn(n)(t)(r))(!1)}}};var Ga=function(n){return function(){return n}},Qa=function(n){return function(t){return function(){return t(n())()}}};var yt=function(n){return function(t){return function(r){return c(n.Bind1())(t)(function(e){return c(n.Bind1())(r)(function(o){return i(n.Applicative0())(e(o))})})}}};var Ka=function(n){return Math.min(Math.abs(n),2147483647)},Ya=function(n){return function(t){return t===0?0:t>0?Math.floor(n/t):-Math.floor(n/-t)}},Xa=function(n){return function(t){if(t===0)return 0;var r=Math.abs(t);return(n%r+r)%r}};var Za=function(n){return function(t){return n-t|0}};var nu=function(n){return function(t){return n+t|0}},tu=function(n){return function(t){return n*t|0}};var Lo={add:nu,zero:0,mul:tu,one:1};var ru={sub:Za,Semiring0:function(){return Lo}};var eu={Ring0:function(){return ru}};var He=function(n){return n.mod};var Ue={degree:Ka,div:Ya,mod:Xa,CommutativeRing0:function(){return eu}},$e=function(n){return n.div};var nn=function(){function n(){}return n.value=new n,n}(),tn=function(){function n(){}return n.value=new n,n}(),gn=function(){function n(){}return n.value=new n,n}();var B=function(n){return n.append};var yn=function(n){return n.mempty};var iu=function(n,t,r){var e=0,o;return function(a){if(e===2)return o;if(e===1)throw new ReferenceError(n+" was needed before it finished initializing (module "+t+", line "+a+")",t,a);return e=1,o=r(),e=2,o}},Nt={Applicative0:function(){return At},Bind1:function(){return $n}},$n={bind:Qa,Apply0:function(){return cu(0)}},At={pure:Ga,Apply0:function(){return cu(0)}},fu=iu("functorEffect","Effect",function(){return{map:jr(At)}}),cu=iu("applyEffect","Effect",function(){return{apply:yt(Nt),Functor0:function(){return fu(0)}}}),q=fu(20);var Je={liftEffect:Z(an),Monad0:function(){return Nt}},I=function(n){return n.liftEffect};function lu(n){return function(t){return function(){return setTimeout(t,n)}}}function su(n){return function(){clearTimeout(n)}}function pu(n){return function(t){return function(){return setInterval(t,n)}}}function _u(n){return function(){clearInterval(n)}}var mu=function(n){return function(t){return function(r){return function(e){return function(o){return e<o?n:e===o?t:r}}}}};var du=mu;var vu=mu;var Rt=function(){return{compare:du(nn.value)(gn.value)(tn.value),Eq0:function(){return Kr}}}(),qe=function(){return{compare:vu(nn.value)(gn.value)(tn.value),Eq0:function(){return ja}}}();var Oe=lu,Du=pu;var Cu=su,gu=_u;var yu=String.fromCharCode(65535),bu=String.fromCharCode(0),Xf=Number.POSITIVE_INFINITY,Zf=Number.NEGATIVE_INFINITY;var _t=function(n){return n.top};var ke={top:2147483647,bottom:-2147483648,Ord0:function(){return Rt}},fr={top:yu,bottom:bu,Ord0:function(){return qe}};var mt=function(n){return n.bottom};var Tu=function(n){return n.toString()};var So={show:Tu};var X=function(n){return n.show};var b=function(){function n(){}return n.value=new n,n}(),D=function(){function n(t){this.value0=t}return n.create=function(t){return new n(t)},n}();var Vt=function(n){return function(t){return function(r){if(r instanceof b)return n;if(r instanceof D)return t(r.value0);throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): "+[n.constructor.name,t.constructor.name,r.constructor.name])}}},Ir=Vt(!0)(j(!1));var ct={map:function(n){return function(t){return t instanceof D?new D(n(t.value0)):b.value}}};var cr=function(){return function(n){if(n instanceof D)return n.value0;throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): "+[n.constructor.name])}};var Fn=function(){function n(t){this.value0=t}return n.create=function(t){return new n(t)},n}(),wn=function(){function n(t){this.value0=t}return n.create=function(t){return new n(t)},n}();var Gn=function(n){return n.ask};var lr={map:function(n){return function(t){return n(t)}}};var xu=function(n){return function(){return{value:n}}};var Tt=function(n){return function(){return n.value}};var Gt=function(n){return function(t){return function(){t.value=n}}};var pr=xu;var On=function(){function n(t){this.value0=t}return n.create=function(t){return new n(t)},n}(),Un=function(){function n(t){this.value0=t}return n.create=function(t){return new n(t)},n}(),dt=function(n){return n.tailRecM};var wr={tailRecM:function(n){return function(t){var r=function(e){if(e instanceof Un)return e.value0;throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 113, column 30 - line 113, column 44): "+[e.constructor.name])};return function(){var o=Nn($n)(pr)(n(t))();return function(){for(;!function(){var d=Tt(o)();if(d instanceof On){var F=n(d.value0)();return Gt(F)(o)(),!1}if(d instanceof Un)return!0;throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 104, column 22 - line 109, column 28): "+[d.constructor.name])}(););return{}}(),u(q)(r)(Tt(o))()}}},Monad0:function(){return Nt}};var Q=function(){function n(t,r){this.value0=t,this.value1=r}return n.create=function(t){return function(r){return new n(t,r)}},n}();var rt=function(n){return n.value1};var lt=function(n){return n.value0};var Mn=function(n){return n.lift};var f=function(n){return n};var wu=function(){return f};var mr=wu;var Qe=function(n){return n};var ee=function(n){return n};var Nu={lift:function(n){return function(t){return Qe(j(t))}}},Au=function(n){return function(t){return function(r){return n(t(r))}}},oe=function(n){return{map:function(){var t=u(n);return function(r){return Au(t(r))}}()}};var Ke=function(n){return{apply:function(t){return function(r){return function(e){return mn(n)(t(e))(r(e))}}},Functor0:function(){return oe(n.Functor0())}}},ae=function(n){return{bind:function(t){return function(r){return function(e){return c(n)(t(e))(function(o){var a=r(o);return a(e)})}}},Apply0:function(){return Ke(n.Apply0())}}};var Ar=function(n){return{pure:function(){var t=i(n);return function(r){return Qe(j(t(r)))}}(),Apply0:function(){return Ke(n.Apply0())}}},vr=function(n){return{Applicative0:function(){return Ar(n.Applicative0())},Bind1:function(){return ae(n.Bind1())}}},Ye=function(n){return{ask:i(n.Applicative0()),Monad0:function(){return vr(n)}}};var ue=function(n){return{liftEffect:function(){var t=Mn(Nu)(n.Monad0()),r=I(n);return function(e){return t(r(e))}}(),Monad0:function(){return vr(n.Monad0())}}},Xe=function(n){return{tailRecM:function(t){return function(r){var e=function(o){return function(a){var d=t(a);return Nn(n.Monad0().Bind1())(i(n.Monad0().Applicative0()))(d(o))}};return function(o){return dt(n)(e(o))(r)}}},Monad0:function(){return vr(n.Monad0())}}};var ie=function(n){return n.bimap};var no={bimap:function(n){return function(t){return function(r){if(r instanceof Fn)return new Fn(n(r.value0));if(r instanceof wn)return new wn(t(r.value0));throw new Error("Failed pattern match at Data.Bifunctor (line 32, column 1 - line 34, column 36): "+[n.constructor.name,t.constructor.name,r.constructor.name])}}}};var fe=f,Ru=f;var Pu=function(n){return function(t){return function(r){for(var e=t,o=r.length,a=o-1;a>=0;a--)e=n(r[a])(e);return e}}},Hu=function(n){return function(t){return function(r){for(var e=t,o=r.length,a=0;a<o;a++)e=n(e)(r[a]);return e}}};var Kt=function(n){return n.foldr};var pe=function(n){return function(t){return function(r){return Kt(t)(function(){var e=Se(n.Apply0());return function(o){return e(r(o))}}())(i(n)(void 0))}}};var ju=function(n){return function(t){return function(r){return Kt(n)(function(e){return function(o){return B(t.Semigroup0())(r(e))(o)}})(yn(t))}}},Kn={foldr:Pu,foldl:Hu,foldMap:function(n){return ju(Kn)(n)}};var Gu=function(){function n(o){return[o]}function t(o){return function(a){return[o,a]}}function r(o){return function(a){return function(d){return[o,a,d]}}}function e(o){return function(a){return o.concat(a)}}return function(o){return function(a){return function(d){return function(F){return function(R){function on(z,In){switch(In-z){case 0:return d([]);case 1:return a(n)(F(R[z]));case 2:return o(a(t)(F(R[z])))(F(R[z+1]));case 3:return o(o(a(r)(F(R[z])))(F(R[z+1])))(F(R[z+2]));default:var An=z+Math.floor((In-z)/4)*2;return o(a(e)(on(z,An)))(on(An,In))}}return on(0,R.length)}}}}}}();var Dr=function(n){return n.traverse};var Tl=function(n){return function(t){return Dr(n)(t)(Z(an))}},Sr={traverse:function(n){return Gu(mn(n.Apply0()))(u(n.Apply0().Functor0()))(i(n))},sequence:function(n){return Tl(Sr)(n)},Functor0:function(){return Mt},Foldable1:function(){return Kn}},Wr=function(n){return n.sequence};var Cr=function(){var n={},t="Pure",r="Throw",e="Catch",o="Sync",a="Async",d="Bind",F="Bracket",R="Fork",on="Sequential",z="Map",In="Apply",An="Alt",ln="Cons",$t="Resume",Eo="Release",$a="Finalizer",ho="Finalized",Ja="Forked",wm="Fiber",Lm="Thunk";function h(v,U,vn,V){this.tag=v,this._1=U,this._2=vn,this._3=V}function at(v){var U=function(vn,V,C){return new h(v,vn,V,C)};return U.tag=v,U}function Fo(v){return new h(t,void 0)}function Ef(v){try{v()}catch(U){setTimeout(function(){throw U},0)}}function hf(v,U,vn){try{return U(vn())}catch(V){return v(V)}}function Ff(v,U,vn){try{return U(vn)()}catch(V){return vn(v(V))(),Fo}}var xe=function(){var v=1024,U=0,vn=0,V=new Array(v),C=!1;function l(){var k;for(C=!0;U!==0;)U--,k=V[vn],V[vn]=void 0,vn=(vn+1)%v,k();C=!1}return{isDraining:function(){return C},enqueue:function(k){var x,_n;U===v&&(_n=C,l(),C=_n),V[(vn+U)%v]=k,U++,C||l()}}}();function Mf(v){var U={},vn=0,V=0;return{register:function(C){var l=vn++;C.onComplete({rethrow:!0,handler:function(k){return function(){V--,delete U[l]}}})(),U[l]=C,V++},isEmpty:function(){return V===0},killAll:function(C,l){return function(){if(V===0)return l();var k=0,x={};function _n(K){x[K]=U[K].kill(C,function(Tn){return function(){delete x[K],k--,v.isLeft(Tn)&&v.fromLeft(Tn)&&setTimeout(function(){throw v.fromLeft(Tn)},0),k===0&&l()}})()}for(var Rn in U)U.hasOwnProperty(Rn)&&(k++,_n(Rn));return U={},vn=0,V=0,function(K){return new h(o,function(){for(var Tn in x)x.hasOwnProperty(Tn)&&x[Tn]()})}}}}}var hr=0,Vn=1,Ie=2,we=3,Le=4,Xn=5,zr=6;function Mo(v,U,vn){var V=0,C=hr,l=vn,k=null,x=null,_n=null,Rn=null,K=null,Tn=0,or=0,ut=null,Jt=!0;function qt(g){for(var y,S,W;;)switch(y=null,S=null,W=null,C){case Ie:C=Vn;try{l=_n(l),Rn===null?_n=null:(_n=Rn._1,Rn=Rn._2)}catch(Wn){C=Xn,k=v.left(Wn),l=null}break;case we:v.isLeft(l)?(C=Xn,k=l,l=null):_n===null?C=Xn:(C=Ie,l=v.fromRight(l));break;case Vn:switch(l.tag){case d:_n&&(Rn=new h(ln,_n,Rn)),_n=l._2,C=Vn,l=l._1;break;case t:_n===null?(C=Xn,l=v.right(l._1)):(C=Ie,l=l._1);break;case o:C=we,l=hf(v.left,v.right,l._1);break;case a:C=Le,l=Ff(v.left,l._1,function(Wn){return function(){V===g&&(V++,xe.enqueue(function(){V===g+1&&(C=we,l=Wn,qt(V))}))}});return;case r:C=Xn,k=v.left(l._1),l=null;break;case e:_n===null?K=new h(ln,l,K,x):K=new h(ln,l,new h(ln,new h($t,_n,Rn),K,x),x),_n=null,Rn=null,C=Vn,l=l._1;break;case F:Tn++,_n===null?K=new h(ln,l,K,x):K=new h(ln,l,new h(ln,new h($t,_n,Rn),K,x),x),_n=null,Rn=null,C=Vn,l=l._1;break;case R:C=we,y=Mo(v,U,l._2),U&&U.register(y),l._1&&y.run(),l=v.right(y);break;case on:C=Vn,l=If(v,U,l._1);break}break;case Xn:if(_n=null,Rn=null,K===null)C=zr,l=x||k||l;else switch(y=K._3,W=K._1,K=K._2,W.tag){case e:x&&x!==y&&Tn===0?C=Xn:k&&(C=Vn,l=W._2(v.fromLeft(k)),k=null);break;case $t:x&&x!==y&&Tn===0||k?C=Xn:(_n=W._1,Rn=W._2,C=Ie,l=v.fromRight(l));break;case F:Tn--,k===null&&(S=v.fromRight(l),K=new h(ln,new h(Eo,W._2,S),K,y),(x===y||Tn>0)&&(C=Vn,l=W._3(S)));break;case Eo:K=new h(ln,new h(ho,l,k),K,x),C=Vn,x&&x!==y&&Tn===0?l=W._1.killed(v.fromLeft(x))(W._2):k?l=W._1.failed(v.fromLeft(k))(W._2):l=W._1.completed(v.fromRight(l))(W._2),k=null,Tn++;break;case $a:Tn++,K=new h(ln,new h(ho,l,k),K,x),C=Vn,l=W._1;break;case ho:Tn--,C=Xn,l=W._1,k=W._2;break}break;case zr:for(var Cn in ut)ut.hasOwnProperty(Cn)&&(Jt=Jt&&ut[Cn].rethrow,Ef(ut[Cn].handler(l)));ut=null,x&&k?setTimeout(function(){throw v.fromLeft(k)},0):v.isLeft(l)&&Jt&&setTimeout(function(){if(Jt)throw v.fromLeft(l)},0);return;case hr:C=Vn;break;case Le:return}}function Dn(g){return function(){if(C===zr)return Jt=Jt&&g.rethrow,g.handler(l)(),function(){};var y=or++;return ut=ut||{},ut[y]=g,function(){ut!==null&&delete ut[y]}}}function T(g,y){return function(){if(C===zr)return y(v.right(void 0))(),function(){};var S=Dn({rethrow:!1,handler:function(){return y(v.right(void 0))}})();switch(C){case hr:x=v.left(g),C=zr,l=x,qt(V);break;case Le:x===null&&(x=v.left(g)),Tn===0&&(C===Le&&(K=new h(ln,new h($a,l(g)),K,x)),C=Xn,l=null,k=null,qt(++V));break;default:x===null&&(x=v.left(g)),Tn===0&&(C=Xn,l=null,k=null)}return S}}function L(g){return function(){var y=Dn({rethrow:!1,handler:g})();return C===hr&&qt(V),y}}return{kill:T,join:L,onComplete:Dn,isSuspended:function(){return C===hr},run:function(){C===hr&&(xe.isDraining()?qt(V):xe.enqueue(function(){qt(V)}))}}}function xf(v,U,vn,V){var C=0,l={},k=0,x={},_n=new Error("[ParAff] Early exit"),Rn=null,K=n;function Tn(Dn,T,L){var g=T,y=null,S=null,W=0,Cn={},Wn,Vr;n:for(;;)switch(Wn=null,g.tag){case Ja:if(g._3===n&&(Wn=l[g._1],Cn[W++]=Wn.kill(Dn,function(wf){return function(){W--,W===0&&L(wf)()}})),y===null)break n;g=y._2,S===null?y=null:(y=S._1,S=S._2);break;case z:g=g._2;break;case In:case An:y&&(S=new h(ln,y,S)),y=g,g=g._1;break}if(W===0)L(v.right(void 0))();else for(Vr=0,Wn=W;Vr<Wn;Vr++)Cn[Vr]=Cn[Vr]();return Cn}function or(Dn,T,L){var g,y,S,W,Cn,Wn;v.isLeft(Dn)?(g=Dn,y=null):(y=Dn,g=null);n:for(;;){if(S=null,W=null,Cn=null,Wn=null,Rn!==null)return;if(T===null){V(g||y)();return}if(T._3!==n)return;switch(T.tag){case z:g===null?(T._3=v.right(T._1(v.fromRight(y))),y=T._3):T._3=g;break;case In:if(S=T._1._3,W=T._2._3,g){if(T._3=g,Cn=!0,Wn=k++,x[Wn]=Tn(_n,g===S?T._2:T._1,function(){return function(){delete x[Wn],Cn?Cn=!1:L===null?or(g,null,null):or(g,L._1,L._2)}}),Cn){Cn=!1;return}}else{if(S===n||W===n)return;y=v.right(v.fromRight(S)(v.fromRight(W))),T._3=y}break;case An:if(S=T._1._3,W=T._2._3,S===n&&v.isLeft(W)||W===n&&v.isLeft(S))return;if(S!==n&&v.isLeft(S)&&W!==n&&v.isLeft(W))g=y===S?W:S,y=null,T._3=g;else if(T._3=y,Cn=!0,Wn=k++,x[Wn]=Tn(_n,y===S?T._2:T._1,function(){return function(){delete x[Wn],Cn?Cn=!1:L===null?or(y,null,null):or(y,L._1,L._2)}}),Cn){Cn=!1;return}break}L===null?T=null:(T=L._1,L=L._2)}}function ut(Dn){return function(T){return function(){delete l[Dn._1],Dn._3=T,or(T,Dn._2._1,Dn._2._2)}}}function Jt(){var Dn=Vn,T=vn,L=null,g=null,y,S;n:for(;;)switch(y=null,S=null,Dn){case Vn:switch(T.tag){case z:L&&(g=new h(ln,L,g)),L=new h(z,T._1,n,n),T=T._2;break;case In:L&&(g=new h(ln,L,g)),L=new h(In,n,T._2,n),T=T._1;break;case An:L&&(g=new h(ln,L,g)),L=new h(An,n,T._2,n),T=T._1;break;default:S=C++,Dn=Xn,y=T,T=new h(Ja,S,new h(ln,L,g),n),y=Mo(v,U,y),y.onComplete({rethrow:!1,handler:ut(T)})(),l[S]=y,U&&U.register(y)}break;case Xn:if(L===null)break n;L._1===n?(L._1=T,Dn=Vn,T=L._2,L._2=n):(L._2=T,T=L,g===null?L=null:(L=g._1,g=g._2))}for(K=T,S=0;S<C;S++)l[S].run()}function qt(Dn,T){Rn=v.left(Dn);var L;for(var g in x)if(x.hasOwnProperty(g)){L=x[g];for(g in L)L.hasOwnProperty(g)&&L[g]()}x=null;var y=Tn(Dn,K,T);return function(S){return new h(a,function(W){return function(){for(var Cn in y)y.hasOwnProperty(Cn)&&y[Cn]();return Fo}})}}return Jt(),function(Dn){return new h(a,function(T){return function(){return qt(Dn,T)}})}}function If(v,U,vn){return new h(a,function(V){return function(){return xf(v,U,vn,V)}})}return h.EMPTY=n,h.Pure=at(t),h.Throw=at(r),h.Catch=at(e),h.Sync=at(o),h.Async=at(a),h.Bind=at(d),h.Bracket=at(F),h.Fork=at(R),h.Seq=at(on),h.ParMap=at(z),h.ParApply=at(In),h.ParAlt=at(An),h.Fiber=Mo,h.Supervisor=Mf,h.Scheduler=xe,h.nonCanceler=Fo,h}(),Il=Cr.Pure,wl=Cr.Throw;var Ll=Cr.Sync;var ci=Cr.Async;var Nl=function(){function n(r,e){return r===0&&typeof setImmediate<"u"?setImmediate(e):setTimeout(e,r)}function t(r,e){return r===0&&typeof clearImmediate<"u"?clearImmediate(e):clearTimeout(e)}return function(r,e){return Cr.Async(function(o){return function(){var a=n(e,o(r()));return function(){return Cr.Sync(function(){return r(t(e,a))})}}})}}(),Al=Cr.Seq;var Kl=function(){function n(t,r){this.value0=t,this.value1=r}return n.create=function(t){return function(r){return new n(t,r)}},n}(),Pr=function(){function n(t){this.value0=t}return n.create=function(t){return new n(t)},n}(),ve=function(){function n(t){this.value0=t}return n.create=function(t){return new n(t)},n}(),pi=function(n){return{lift:function(t){return function(r){return new Pr(function(e){return u(t.Bind1().Apply0().Functor0())(Fn.create)(r)})}}}};var fo=function(n){return function(t){return new ve(Ru(new Kl(n,t)))}},Xo=function(n){return function(t){return{map:function(r){return function(e){if(e instanceof Pr)return new Pr(function(o){return u(t)(ie(no)(r)(u(n)(u(Xo(n)(t))(r))))(e.value0(void 0))});if(e instanceof ve)return fe(function(o){return fo(o.value0)(function(){var a=u(Xo(n)(t))(r);return function(d){return a(o.value1(d))}}())})(e.value0);throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 64, column 1 - line 66, column 71): "+[r.constructor.name,e.constructor.name])}}}}};var _i=function(n){return function(t){return{Applicative0:function(){return di(n)(t)},Bind1:function(){return de(n)(t)}}}},de=function(n){return function(t){return{bind:function(r){return function(e){return r instanceof ve?fe(function(o){return fo(o.value0)(function(a){return fo(function(d){return o.value1(a)})(e)})})(r.value0):fo(function(o){return r})(e)}},Apply0:function(){return mi(n)(t)}}}},mi=function(n){return function(t){return{apply:yt(_i(n)(t)),Functor0:function(){return Xo(n)(t.Bind1().Apply0().Functor0())}}}},di=function(n){return function(t){return{pure:function(r){return new Pr(function(e){return i(t.Applicative0())(new Fn(r))})},Apply0:function(){return mi(n)(t)}}}};var Yl=function(n){return function(t){var r=function(e){if(e instanceof Pr)return u(t.Monad0().Bind1().Apply0().Functor0())(Un.create)(e.value0(void 0));if(e instanceof ve)return fe(function(o){var a=o.value0(void 0);if(a instanceof Pr)return c(t.Monad0().Bind1())(a.value0(void 0))(function(d){if(d instanceof Fn)return i(t.Monad0().Applicative0())(new On(o.value1(d.value0)));if(d instanceof wn)return i(t.Monad0().Applicative0())(new Un(new wn(u(n)(function(F){return c(de(n)(t.Monad0()))(F)(o.value1)})(d.value0))));throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 57, column 22 - line 59, column 69): "+[d.constructor.name])});if(a instanceof ve)return fe(function(d){return i(t.Monad0().Applicative0())(new On(c(de(n)(t.Monad0()))(d.value0(void 0))(function(F){return c(de(n)(t.Monad0()))(d.value1(F))(o.value1)})))})(a.value0);throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 55, column 7 - line 62, column 60): "+[a.constructor.name])})(e.value0);throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 51, column 3 - line 51, column 75): "+[e.constructor.name])};return dt(t)(r)}},vi=function(n){return function(t){return function(r){var e=function(o){if(o instanceof Fn)return i(t.Monad0().Applicative0())(new Un(o.value0));if(o instanceof wn)return u(t.Monad0().Bind1().Apply0().Functor0())(On.create)(r(o.value0));throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 144, column 3 - line 144, column 63): "+[o.constructor.name])};return dt(t)(wo(t.Monad0().Bind1())(e)(Yl(n)(t)))}}};var Di=function(n){return function(t){return{tailRecM:function(r){var e=function(o){return c(de(n)(t))(r(o))(function(a){if(a instanceof On)return e(a.value0);if(a instanceof Un)return i(di(n)(t))(a.value0);throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 87, column 15 - line 89, column 25): "+[a.constructor.name])})};return e},Monad0:function(){return _i(n)(t)}}}};var bi=function(n){return function(t){return function(r){return function(e){return function(o){return function(a){for(var d=[],F=a;;){var R=o(F);if(n(R))return d;var on=t(R);d.push(r(on)),F=e(on)}}}}}}};var Ti=function(n){return function(t){return function(r){return function(e){return function(o){return function(a){for(var d=[],F=a;;){var R=o(F);d.push(r(R));var on=e(R);if(n(on))return d;F=t(on)}}}}}}};var hi={unfoldr1:Ti(Ir)(cr())(lt)(rt)};var De=function(n){return n.unfoldr};var ta={unfoldr:bi(Ir)(cr())(lt)(rt),Unfoldable10:function(){return hi}};var Cs=function(n){return function(t){var r=function(e){return vi(lr)(e)(function(){var o=i(e.Monad0().Applicative0()),a=mr();return function(d){return o(a(d))}}())};return r(n)(t(Di(lr)(n.Monad0()))(Mn(pi(lr))(n.Monad0()))(r(n)))}},gs=function(n){return function(t){return function(r){return function(e){return Cs(t)(function(o){return function(a){return function(d){return pe(o.Monad0().Applicative0())(n)(function(F){return a(r(F))})(e)}}})}}}};var Yt=function(n){return function(t){return Bn(gs(n)(t))}};var ea=n=>()=>{let t={callbacks:new Set,dependencies:new Set};return t.effect=n(t),t},oa=()=>new Set,aa=n=>t=>()=>{n.dependencies.add(t),t.add(n)},Mi=n=>t=>()=>{n.dependencies.delete(t),t.delete(n)},lo=n=>()=>{n.dependencies.forEach(t=>{Mi(n)(t)()})},ua=n=>t=>()=>{n.callbacks.add(t)},ia=n=>()=>{n.callbacks.clear()},fa=n=>()=>[...n.callbacks],ca=n=>n.effect,la=n=>()=>[...n];var sa=function(n){return function(){var r=fa(n)();return Yt(Kn)(wr)(r)(function(e){return i(At)(void 0)})(),ia(n)()}};var Ii=Xe(wr),wi=vr(Nt),Ln=ue(Je),Li=Ye(Nt),kn=oe(q),bn=ae($n);var w=Ar(At);var xi=function(n){return function(t){return ee(t)(new D(n))}},so=function(n){return function(t){return function(r){return I(n)(function(){var o=pr(r)(),a=oa(),d=function(R){return I(Ln)(function(){var z=Tt(o)(),In=R(z),An=Pe(t)(z)(In);if(An){var ln=la(a)();return Yt(Kn)(wr)(ln)(function($t){return function(){return lo($t)(),sa($t)()}})(),Gt(In)(o)(),Yt(Kn)(wr)(ln)(function($t){return ca($t)})()}return void 0})},F=c(bn)(Gn(Li))(function(R){return Pn(Hn)(bn)(I(Ln)(function(){if(R instanceof D)return aa(R.value0)(a);if(R instanceof b)return i(At)(void 0);throw new Error("Failed pattern match at Jelly.Data.Jelly (line 55, column 18 - line 57, column 29): "+[R.constructor.name])}()))(function(){return I(Ln)(Tt(o))})});return new Q(F,d)})}}};var Dt=function(n){return ee(n)(b.value)},pa=function(n){return c(bn)(Gn(Li))(function(t){var r=Dt(n);if(t instanceof D)return I(Ln)(ua(t.value0)(r));if(t instanceof b)return i(w)(void 0);throw new Error("Failed pattern match at Jelly.Data.Jelly (line 95, column 3 - line 98, column 25): "+[t.constructor.name])})};var Ni=function(n){return c(bn)(I(Ln)(ea(function(t){return xi(t)(n)})))(function(t){return Pn(Hn)(bn)(I(Ln)(xi(t)(n)))(function(){var r=function(){return lo(t)(),sa(t)()};return pa(I(Ln)(r))})})};var Ts=function(n){return n},_a=Xe(Ii);var Yn=ue(Ln),po=Ye(wi),Ai=oe(kn),$=ae(bn);var Xt=Ar(w),_o=function(n){return function(t){return ee(t)(n)}},Hr=function(n){return Ts(Qe(j(n)))};var ma=function(n){return function(t){return t.join(n)}};var da=function(){function n(t,r){this.value0=t,this.value1=r}return n.create=function(t){return function(r){return new n(t,r)}},n}(),va=function(){function n(t,r){this.value0=t,this.value1=r}return n.create=function(t){return function(r){return new n(t,r)}},n}(),ge=function(){return va.create}(),Fs=function(){return da.create}(),pn=function(n){return Fs("class")(u(kn)(ma(" "))(Wr(Sr)(w)(n)))};var Ht=function(n){return so(Yn)(n)};var vo=function(n){return Hr(Ni(n))};var gr=function(n){return function(t){return function(){return t[n]}}},xs=gr("URL"),Is=gr("documentURI"),ws=gr("origin"),Ls=gr("compatMode"),Ns=gr("characterSet"),As=gr("contentType");var Rs=gr("documentElement");function Da(n){return function(t){return function(){return t.createElement(n)}}}function Ca(n){return function(t){return function(){return t.createTextNode(n)}}}function Si(n,t,r){return n==null?t:r(n)}var hn=function(n){return Si(n,b.value,D.create)};var Do=function(n){return function(t){return t[n]}},Js=Do("namespaceURI"),qs=Do("prefix"),Os=Do("localName"),ks=Do("tagName");function ga(n){return function(t){return function(r){return function(){r.setAttribute(n,t)}}}}var Co=function(n){return function(t){return function(){return t[n]}}},zs=Co("children"),Vs=Co("firstElementChild"),js=Co("lastElementChild"),Gs=Co("childElementCount");var ya=f,Hi=f;var Ct=function(n){return function(t){return function(){return t[n]}}};var Zs=Ct("baseURI"),np=Ct("ownerDocument"),tp=Ct("parentNode"),rp=Ct("parentElement");var ep=Ct("childNodes"),op=Ct("firstChild"),ap=Ct("lastChild"),up=Ct("previousSibling"),ip=Ct("nextSibling"),fp=Ct("nodeValue");var cp=Ct("textContent");function ba(n){return function(t){return function(){t.textContent=n}}}function Ta(n){return function(t){return function(r){return function(){r.insertBefore(n,t)}}}}function ye(n){return function(t){return function(){t.appendChild(n)}}}function Ea(n){return function(t){return function(){t.removeChild(n)}}}function $r(n){return n.charCodeAt(0)}function Ui(n){return String.fromCharCode(n)}var be=function(n){return n.toEnum};var yr=function(n){return n.fromEnum},$i=function(n){return function(t){return function(r){return function(e){var o=be(n)(e);if(o instanceof D)return o.value0;if(o instanceof b){var a=e<yr(n)(mt(n.Bounded0()));return a?t:r}throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): "+[o.constructor.name])}}}};var Jr=function(n){return function(t){return function(r){return n(t(r)+1|0)}}},qr=function(n){return function(t){return function(r){return n(t(r)-1|0)}}};var ha=function(n){return n>=mt(ke)&&n<=_t(ke)?new D(Ui(n)):b.value},_p={succ:Jr(ha)($r),pred:qr(ha)($r),Ord0:function(){return qe}};var br=function(){return{cardinality:$r(_t(fr))-$r(mt(fr))|0,toEnum:ha,fromEnum:$r,Bounded0:function(){return fr},Enum1:function(){return _p}}}();var qi=f;function Fa(n){return function(){return function(t){return n(t)()}}}function Ma(n){return function(t){return function(r){return function(e){return function(){return e.addEventListener(n,t,r)}}}}}var nr=function(){return window};function Oi(n){return function(){return n.body}}var xa=f;var ki=function(){var n=u(q)(hn);return function(t){return n(Oi(t))}}();var zi=f;function Ia(n){return function(t){return function(){t.href=n}}}function Or(n){return function(){return n.document}}function wa(n){return function(){return n.location}}var tr=function(n){return c($)(I(Yn)(u(q)(qi)(Nn($n)(function(){var t=Ca("");return function(r){return t(xa(r))}}())(Nn($n)(Or)(nr)))))(function(t){return Pn(Hn)($)(vo(c(bn)(n)(function(r){return Pn(Hn)(bn)(I(Ln)(ba(r)(t)))(function(){return i(w)(void 0)})})))(function(){return i(Xt)(t)})})},D_=function(n){return function(t){if(t instanceof da)return vo(c(bn)(t.value1)(function(r){return I(Ln)(ga(t.value0)(r)(n))}));if(t instanceof va)return c($)(I(Yn)(Fa(function(r){return Dt(t.value1(r))})))(function(r){return I(Yn)(Ma(t.value0)(r)(!1)(Hi(n)))});throw new Error("Failed pattern match at Jelly.HTML (line 30, column 3 - line 40, column 30): "+[t.constructor.name])}};var C_=tr(i(w)("")),Aa=function(n){return function(t){return c($)(Hr(n))(function(r){return r?t:C_})}},g_=function(n){return function(t){return c($)(I(Yn)(pr(b.value)))(function(r){return c($)(Gn(po))(function(e){return vo(c(bn)(I(Ln)(Tt(r)))(function(o){return c(bn)(_o(e)(t))(function(a){return Pn(Hn)(bn)(I(Ln)(function(){if(o instanceof b)return ye(a)(n);if(o instanceof D)return function(){return Ta(a)(o.value0)(n)(),Ea(o.value0)(n)()};throw new Error("Failed pattern match at Jelly.HTML (line 53, column 16 - line 58, column 39): "+[o.constructor.name])}()))(function(){return I(Ln)(Gt(new D(a))(r))})})}))})})}},Te=function(n){return function(t){return function(r){return c($)(I(Yn)(Nn($n)(function(){var e=Da(n);return function(o){return e(xa(o))}}())(Nn($n)(Or)(nr))))(function(e){return Pn(Hn)($)(Yt(Kn)(_a)(t)(D_(e)))(function(){return Pn(Hn)($)(Yt(Kn)(_a)(r)(g_(ya(e))))(function(){return i(Xt)(ya(e))})})})}}};var Tr=Te("button"),Sn=Te("div");var kr=function(n){return c($)(Ht(Ot)(!1))(function(t){return c($)(I(Yn)(Oe(100)(Dt(t.value1(function(r){return!0})))))(function(){return Sn([pn([i(w)("transition-all"),Gr(bn)(t.value0)(i(w)("scale-100 opacity-100"))(i(w)("scale-50 opacity-0"))])])([n])})})};var Ki=Gn(po);var rr=function(){function n(){}return n.value=new n,n}(),Er=function(){function n(){}return n.value=new n,n}(),Yi={eq:function(n){return function(t){return n instanceof rr&&t instanceof rr||n instanceof Er&&t instanceof Er}}},Ra=c($)(Ki)(function(n){return i(Xt)(n.colorMode)}),Ut=function(n){var t=function(r){return r.text+(" "+r.background)};return{primary:t(n.primary),highlight:t(n.highlight),reverse:t(n.reverse)}},b_=function(n){if(n instanceof rr)return{primary:{text:"text-slate-900",background:"bg-white"},highlight:{text:"text-slate-900",background:"bg-pink-500"},reverse:{text:"text-white",background:"bg-slate-900"}};if(n instanceof Er)return{primary:{text:"text-white",background:"bg-slate-900"},highlight:{text:"text-white",background:"bg-pink-600"},reverse:{text:"text-slate-900",background:"bg-white"}};throw new Error("Failed pattern match at Contexts.ColorMode (line 37, column 18 - line 65, column 6): "+[n.constructor.name])},Br=c($)(Ra)(function(n){return i(Xt)(u(kn)(b_)(n.value0))});var Xi=c($)(Br)(function(n){return c($)(Ht(Kr)(0))(function(t){return kr(Sn([pn([i(w)("h-24 w-24 flex flex-col justify-center items-center relative")])])([Sn([pn([i(w)("h-24 w-24 -rotate-0 hover:rotate-0 transition-all absolute origin-center rounded-md"),Zn(kn)(n)(function(r){return function(e){return e.highlight}(Ut(r))})])])([]),Tr([pn([i(w)("h-24 w-24 hover:rotate-[20deg] transition-all absolute origin-center rounded-md"),Zn(kn)(n)(function(r){return function(e){return e.reverse}(Ut(r))})]),ge("click")(function(r){return t.value1(function(e){return e+1|0})})])([]),Sn([pn([i(w)("flex justify-center items-center text-3xl z-20 relative pointer-events-none transition-colors"),Zn(kn)(n)(function(r){return function(e){return e.reverse}(Ut(r))})])])([tr(u(kn)(X(So))(t.value0))])]))})});var he=function(n){return Te("i")([pn([n])])([])};var Zi=c($)(Br)(function(n){return Sn([pn([i(w)("relative w-56 mt-6 h-5 z-10  flex flex-row justify-center items-end rounded-t-md transition-colors"),Zn(kn)(n)(function(t){return function(r){return r.highlight}(Ut(t))})])])([Sn([pn([i(w)("relative text-5xl z-20 font-AlfaSlabOne font-extrabold transition-colors")])])([tr(i(w)("JELLY"))])])});var F_=typeof Array.from=="function",M_=typeof Symbol<"u"&&Symbol!=null&&typeof Symbol.iterator<"u"&&typeof String.prototype[Symbol.iterator]=="function",x_=typeof String.prototype.fromCodePoint=="function",I_=typeof String.prototype.codePointAt=="function",nf=function(n){return I_?function(t){return t.codePointAt(0)}:n};var tf=function(n){return x_?String.fromCodePoint:n},rf=function(n){return function(t){return M_?function(r){for(var e="",o=r[Symbol.iterator](),a=0;a<t;++a){var d=o.next();if(d.done)return e;e+=d.value}return e}:n(t)}},ef=function(n){return function(t){return F_?function(r){return Array.from(r,t)}:n}};var L_=function(n){return function(t){if(n<1)return[];var r=new Array(n);return r.fill(t)}},N_=function(n){return function(t){for(var r=[],e=0,o=0;o<n;o++)r[e++]=t;return r}},A_=typeof Array.prototype.fill=="function"?L_:N_,R_=function(){function n(o,a){this.head=o,this.tail=a}var t={};function r(o){return function(a){return new n(o,a)}}function e(o){for(var a=[],d=0,F=o;F!==t;)a[d++]=F.head,F=F.tail;return a}return function(o){return function(a){return e(o(r)(t)(a))}}}(),yo=function(n){return n.length};var S_=function(){function n(t,r,e,o,a,d){var F,R,on,z,In,An,ln;for(F=a+(d-a>>1),F-a>1&&n(t,r,o,e,a,F),d-F>1&&n(t,r,o,e,F,d),R=a,on=F,z=a;R<F&&on<d;)In=o[R],An=o[on],ln=r(t(In)(An)),ln>0?(e[z++]=An,++on):(e[z++]=In,++R);for(;R<F;)e[z++]=o[R++];for(;on<d;)e[z++]=o[on++]}return function(t){return function(r){return function(e){var o;return e.length<2?e:(o=e.slice(0),n(t,r,o,e.slice(0),0,e.length),o)}}}}();var J_=function(){function n(t,r,e,o,a,d){var F,R,on,z,In,An,ln;for(F=a+(d-a>>1),F-a>1&&n(t,r,o,e,a,F),d-F>1&&n(t,r,o,e,F,d),R=a,on=F,z=a;R<F&&on<d;)In=o[R],An=o[on],ln=r(t(In)(An)),ln>0?(e[z++]=An,++on):(e[z++]=In,++R);for(;R<F;)e[z++]=o[R++];for(;on<d;)e[z++]=o[on++]}return function(t){return function(r){return function(e){return function(){return e.length<2||n(t,r,e,e.slice(0),0,e.length),e}}}}}();var Pa=function(n){return n};var Fe=function(n){return n.length};var Me=function(n){return function(t){return t.substring(n)}};var er=function(n){return function(t){if(n>=0&&n<t.length)return t.charAt(n);throw new Error("Data.String.Unsafe.charAt: Invalid index.")}};var mf=function(n){return function(t){return(((n-55296|0)*1024|0)+(t-56320|0)|0)+65536|0}};var df=function(n){return 56320<=n&&n<=57343},vf=function(n){return 55296<=n&&n<=56319},Df=function(n){var t=Fe(n);if(t===0)return b.value;if(t===1)return new D({head:yr(br)(er(0)(n)),tail:""});var r=yr(br)(er(1)(n)),e=yr(br)(er(0)(n)),o=vf(e)&&df(r);return o?new D({head:mf(e)(r),tail:Me(2)(n)}):new D({head:e,tail:Me(1)(n)})},Cm=function(n){return u(ct)(function(t){return new Q(t.head,t.tail)})(Df(n))},gm=function(n){return De(ta)(Cm)(n)},ym=function(n){var t=yr(br)(er(0)(n)),r=vf(t)&&Fe(n)>1;if(r){var e=yr(br)(er(1)(n)),o=df(e);return o?mf(t)(e):t}return t},bm=nf(ym),Tm=ef(gm)(bm),bo=function(n){return yo(Tm(n))};var Ha=function(){var n=$i(br)(mt(fr))(_t(fr));return function(t){return Pa(n(t))}}(),Em=function(n){if(n<=65535)return Ha(n);var t=$e(Ue)(n-65536|0)(1024)+55296|0,r=He(Ue)(n-65536|0)(1024)+56320|0;return Ha(t)+Ha(r)};var hm=tf(Em),Cf=function(n){return function(t){if(n<1)return"";var r=Df(t);return r instanceof D?hm(r.value0.head)+Cf(n-1|0)(r.value0.tail):t}},gf=rf(Cf);var To=function(n){return Hr(pa(n))};var Ua=function(n){return c($)(Ht(Qr)("\u25A0"))(function(t){return c($)(Ht(Ot)(!1))(function(r){return c($)(I(Yn)(Du(50)(Dt(c(bn)(t.value0)(function(e){return Gr(bn)(r.value0)(t.value1(j(n)))(Pn(Hn)(bn)(t.value1(j(gf(bo(e))(n)+"\u25A0")))(function(){return ur(w)(bo(e)===bo(n))(r.value1(j(!0)))}))})))))(function(e){return Pn(Hn)($)(To(I(Ln)(gu(e))))(function(){return i(Xt)(new Q(t.value0,r.value0))})})})})};var bf=function(n){return function(t){return function(){var e=Nn($n)(ki)(Nn($n)(Or)(nr))(),o=Dt(_o(n)(t))();if(e instanceof D)return ye(o)(zi(e.value0))();if(e instanceof b)return void 0;throw new Error("Failed pattern match at Jelly.RunComponent (line 23, column 3 - line 25, column 25): "+[e.constructor.name])}}};var Im=c($)(Ua("An easy way to create interactive web apps."))(function(n){return c($)(Ra)(function(t){return c($)(Br)(function(r){return c($)(Ht(Ot)(!1))(function(e){return c($)(I(Yn)(Oe(400)(Dt(e.value1(j(!0))))))(function(o){return Pn(Hn)($)(To(I(Ln)(Cu(o))))(function(){return Sn([pn([i(w)("h-screen w-screen relative text-xl overflow-hidden flex flex-col items-center font-Inconsolata transition-colors"),Zn(kn)(r)(function(a){return function(d){return d.primary}(Ut(a))})])])([Sn([pn([i(w)("py-3 px-8 flex justify-between w-screen")])])([Sn([pn([i(w)("w-12")])])([]),Zi,Tr([pn([i(w)("w-12 rounded-full hover:scale-110 transition-all flex justify-center items-center")]),ge("click")(function(a){return c(bn)(t.value0)(function(d){if(d instanceof rr)return t.value1(Er.value);if(d instanceof Er)return t.value1(rr.value);throw new Error("Failed pattern match at Main (line 68, column 17 - line 70, column 45): "+[d.constructor.name])})})])([he(c(bn)(t.value0)(function(a){if(a instanceof rr)return i(w)("fa-solid fa-sun fa-lg");if(a instanceof Er)return i(w)("fa-solid fa-moon fa-lg");throw new Error("Failed pattern match at Main (line 74, column 17 - line 76, column 56): "+[a.constructor.name])}))])]),Sn([pn([i(w)("h-1 w-screen transition-colors"),Zn(kn)(r)(function(a){return function(d){return d.reverse}(Ut(a))})])])([]),Sn([pn([i(w)("py-6")])])([tr(n.value0)]),Aa(e.value0)(Sn([pn([i(w)("flex-grow flex flex-row items-center justify-between w-full")])])([Tr([pn([i(w)("h-full w-32 hover:-translate-x-1 transition-transform")])])([kr(he(i(w)("fa-xl fa-solid fa-chevron-left")))]),Xi,Tr([pn([i(w)("h-full w-32 hover:translate-x-1 transition-transform")])])([kr(he(i(w)("fa-xl fa-solid fa-chevron-right")))])])),Aa(e.value0)(Sn([pn([i(w)("flex flex-row w-full justify-between items-center p-10")])])([Sn([pn([i(w)("w-12")])])([]),Nn($)(tr)(u(Ai)(lt)(Ua("A Button"))),Tr([pn([i(w)("w-12 h-12 rounded-full hover:scale-110 transition-all flex justify-center items-center")]),ge("click")(function(a){return I(Ln)(Nn($n)(Ia("https://github.com/yukikurage/purescript-jelly-examples"))(Nn($n)(wa)(nr)))})])([kr(he(i(w)("fa-xl fa-brands fa-github")))])]))])})})})})})}),Tf=function(){var t=so(Je)(Yi)(rr.value)();return bf({colorMode:new Q(t.value0,function(r){return t.value1(j(r))})})(Im)()};Tf();})();
