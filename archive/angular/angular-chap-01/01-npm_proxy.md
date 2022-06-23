# 01 NPM derrière un proxy

url dans chrome pour avoir des infos :

```chrome://net-internals/#proxy```



Fichier de config sous windows :

```http://wpad/wpad.dat```

une fois le fichier récupérer regarder en bas :

```csharp
function FindProxyForURL(url, host) {
	if (
		// plain host name - no domain
		isPlainHostName(host)			||
		// direct host fqdn
	    (host == "auth.minfin.fgov.be") ||
		(host == "localhost")	||
		// domains
		dnsDomainIs(host, ".finbel.intra")	||
		dnsDomainIs(host, ".fcinet.org") ||
		dnsDomainIs(host, "debt.agency")	||
		dnsDomainIs(host, "intra.minfin.be")	||
		// networks
		//isInNet(host, "127.0.0.0", "255.0.0.0")	||
		shExpMatch(host,"127.*") ||
		shExpMatch(host, "10.*")
		) 
		return "DIRECT";

    if (dnsDomainIs(host, "fiscoloog.be")				||
		dnsDomainIs(host, "fgov.kluwer.be")			||
		dnsDomainIs(host, "rw.be")				||
		dnsDomainIs(host, "tijdschriftvoornotarissen.be")	||
		dnsDomainIs(host, "fiscologue.be")			||
		dnsDomainIs(host, "fiscalnet.be")
		)
		return "PROXY zabopxp01.finbel.intra:8080";

	if (dnsDomainIs(host, "stradalex.com")
		)
		return "PROXY zabopxp02.finbel.intra:3128";  

	if (dnsDomainIs(host, ".ccncsi.int"))
		return "PROXY ccnbecup1.finbel.intra:1998";
	// c'est celui ci
	return "PROXY proxies.finbel.intra:8080";
}
```

En mode administrateur dans la commande windows :

```shell
# peut être
npm config set registry "http://registry.npmjs.org/"
npm config set strict-ssl false

# surement :
npm config set proxy http://karim.meshoub:thanos5gems@proxies.finbel.intra:8080
npm config set https-proxy http://karim.meshoub:thanos5gems@proxies.finbel.intra:8080

```

