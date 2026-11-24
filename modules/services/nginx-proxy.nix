{
  lib,
  config,
  ...
}:

with lib;
let
  cfg = config.nginxProxy;

  defaultStreamConfig = {
    proxyProtocol = false;
  };
  
  # configOrDefault = port:
  #   if types.isType types.port port then
  #     defaultStreamConfig // { port = port; }
  #   else
  #     defaultStreamConfig // port
  # ;

  genStreamConfig = serverIp: serverCfgs:
    concatStringsSep "\n\n" (
      map (config:
        let
          proxyProtocol =
            if config.proxyProtocol then
              "on"
            else
              "off";
          portStr = toString config.port;
        in
          concatStringsSep "\n" [
            "server {"
            "    listen ${portStr};"
            "    proxy_pass ${serverIp}:${portStr};"
            "    proxy_protocol ${proxyProtocol};"
            "}"
          ]
      )
        serverCfgs
    );
in
{
  options.nginxProxy = {
    enable = mkEnableOption "Whether to enable nginx proxy";
    serverIp = mkOption {
      description = "An ip to forward the requests to";
      type = types.str;
    };
    ports = mkOption {
      type = types.listOf (types.coercedTo types.port (port: { inherit port; }) (types.submodule {
        options = {
          port = mkOption {
            type = types.port;
          };
          proxyProtocol = mkOption {
            type = types.bool;
            default = false;
          };
        };
      }));
    };
  };

  config = mkIf cfg.enable {
    services.nginx = {
      enable = true;
      streamConfig = genStreamConfig cfg.serverIp cfg.ports;
    };
  };
}
