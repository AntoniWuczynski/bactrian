let interception : Bactrian_runtime.Egress_proxy.interception =
  {
    tls =
      (fun ~sim_now:_ ->
        {
          terminate = (fun ~host:_ _ -> Error "no TLS in this test");
          failure = (fun _ -> None);
        });
    bundle = "-----BEGIN CERTIFICATE-----\n";
  }
