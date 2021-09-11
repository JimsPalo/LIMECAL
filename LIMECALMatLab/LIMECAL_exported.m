classdef LIMECAL_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                        matlab.ui.Figure
        ArchivoMenu                     matlab.ui.container.Menu
        GenerarReporteMenu              matlab.ui.container.Menu
        PdfMenu                         matlab.ui.container.Menu
        ReferenciaMenu                  matlab.ui.container.Menu
        TabGroup                        matlab.ui.container.TabGroup
        HiptesisTab                     matlab.ui.container.Tab
        Label                           matlab.ui.control.Label
        Panel                           matlab.ui.container.Panel
        TemperaturaCEditField           matlab.ui.control.NumericEditField
        TemperaturaCLabel               matlab.ui.control.Label
        HieloPanel                      matlab.ui.container.Panel
        ActivarButton_2                 matlab.ui.control.StateButton
        PesoCheckBox                    matlab.ui.control.CheckBox
        EspesormmEditField              matlab.ui.control.NumericEditField
        EspesormmEditFieldLabel         matlab.ui.control.Label
        StandarCheckBox_2               matlab.ui.control.CheckBox
        PesodelhielokgmEditField        matlab.ui.control.NumericEditField
        PesodelhielokgmLabel            matlab.ui.control.Label
        VientoPanel                     matlab.ui.container.Panel
        ReductioncoefficientEditField   matlab.ui.control.NumericEditField
        ReductioncoefficientEditFieldLabel  matlab.ui.control.Label
        ActivarButton                   matlab.ui.control.StateButton
        StandarCheckBox                 matlab.ui.control.CheckBox
        VelocidadCheckBox               matlab.ui.control.CheckBox
        PresinCheckBox                  matlab.ui.control.CheckBox
        VelocidaddevientokmhEditField   matlab.ui.control.NumericEditField
        VelocidaddevientokmhLabel       matlab.ui.control.Label
        PresindevientokgmEditField      matlab.ui.control.NumericEditField
        PresindevientokgmEditFieldLabel  matlab.ui.control.Label
        Generadordehipotesis1Label      matlab.ui.control.Label
        AadirhiptesisButton             matlab.ui.control.Button
        UITable2                        matlab.ui.control.Table
        Tabladetendido                  matlab.ui.container.Tab
        HiptesismsdesfavorableLabel     matlab.ui.control.Label
        UITable3_2                      matlab.ui.control.Table
        DistanciasenmetrosLabel         matlab.ui.control.Label
        TemperaturaenCLabel             matlab.ui.control.Label
        GenerartablaButton              matlab.ui.control.Button
        IntervaloentrevanosSpinner      matlab.ui.control.Spinner
        IntervaloentrevanosSpinnerLabel  matlab.ui.control.Label
        VanomximoEditField              matlab.ui.control.NumericEditField
        VanomximoEditFieldLabel         matlab.ui.control.Label
        VanomnimoEditField              matlab.ui.control.NumericEditField
        VanomnimoEditFieldLabel         matlab.ui.control.Label
        VanoaLabel                      matlab.ui.control.Label
        TemperaturaLabel                matlab.ui.control.Label
        IntervalosdetemperaturaSpinner  matlab.ui.control.Spinner
        IntervalosdetemperaturaSpinnerLabel  matlab.ui.control.Label
        TemperaturamximamaxEditField    matlab.ui.control.NumericEditField
        TemperaturamximamaxEditFieldLabel  matlab.ui.control.Label
        TemperaturamnimaminEditField    matlab.ui.control.NumericEditField
        TemperaturamnimaminEditFieldLabel  matlab.ui.control.Label
        UITable3                        matlab.ui.control.Table
        GrficasTab                      matlab.ui.container.Tab
        PesoaparenteLabel               matlab.ui.control.Label
        MnimaflechaLabel                matlab.ui.control.Label
        MximaflechaLabel                matlab.ui.control.Label
        LongitudmnimaLabel_2            matlab.ui.control.Label
        FlechamnimaLabel                matlab.ui.control.Label
        PesoaparenteLabel_2             matlab.ui.control.Label
        FlechamximaLabel                matlab.ui.control.Label
        ParaunpesodeLabel               matlab.ui.control.Label
        FlechasmximasymnimasCheckBox    matlab.ui.control.CheckBox
        LongituddeconductormEditField   matlab.ui.control.NumericEditField
        LongituddeconductormEditFieldLabel  matlab.ui.control.Label
        FlechamEditField                matlab.ui.control.NumericEditField
        FlechamEditFieldLabel           matlab.ui.control.Label
        PesoaparentekgkmSpinner         matlab.ui.control.Spinner
        PesoaparentekgkmSpinnerLabel    matlab.ui.control.Label
        TensinvrticekgSpinner           matlab.ui.control.Spinner
        TensinvrticekgSpinnerLabel      matlab.ui.control.Label
        VanomSpinner                    matlab.ui.control.Spinner
        VanomSpinnerLabel               matlab.ui.control.Label
        AplicarButton                   matlab.ui.control.Button
        ResultadosLabel                 matlab.ui.control.Label
        DatosusadosenelclculoLabel      matlab.ui.control.Label
        UIAxes                          matlab.ui.control.UIAxes
        InputTab                        matlab.ui.container.Tab
        CalcularcasodeestudioButton     matlab.ui.control.Button
        ConductorseleccionadoLabel      matlab.ui.control.Label
        UITable_2                       matlab.ui.control.Table
        LoadDataButton                  matlab.ui.control.Button
        SeleccioneelnombredelconductordelossiguientetablaLabel  matlab.ui.control.Label
        UITable                         matlab.ui.control.Table
        InformacindelcasodeestudioPanel  matlab.ui.container.Panel
        Hyperlink                       matlab.ui.control.Hyperlink
        ByJimmyPalominoLabel            matlab.ui.control.Label
        ZonaCLamp_3                     matlab.ui.control.Lamp
        ZonaCLamp_3Label                matlab.ui.control.Label
        CabledetierraCheckBox           matlab.ui.control.CheckBox
        FaseCheckBox                    matlab.ui.control.CheckBox
        VanomEditField                  matlab.ui.control.NumericEditField
        VanomEditFieldLabel             matlab.ui.control.Label
        SeguritycoefficientEditField    matlab.ui.control.NumericEditField
        SeguritycoefficientEditFieldLabel  matlab.ui.control.Label
        AlturamsnmmEditField            matlab.ui.control.NumericEditField
        AlturamsnmmEditFieldLabel       matlab.ui.control.Label
        DatosadicionalesLabel           matlab.ui.control.Label
        DatosdelconductorLabel          matlab.ui.control.Label
        CargaderupturakgEditField       matlab.ui.control.NumericEditField
        CargaderupturakgLabel           matlab.ui.control.Label
        PesokgkmEditField               matlab.ui.control.NumericEditField
        PesokgkmEditFieldLabel          matlab.ui.control.Label
        DilatacinEditField              matlab.ui.control.NumericEditField
        DilatacinEditFieldLabel         matlab.ui.control.Label
        ElasticidadEkgmmEditField       matlab.ui.control.NumericEditField
        ElasticidadEkgmmLabel           matlab.ui.control.Label
        SeccintransversalmmEditField    matlab.ui.control.NumericEditField
        SeccintransversalmmLabel        matlab.ui.control.Label
        DimetrommEditField              matlab.ui.control.NumericEditField
        DimetrommEditFieldLabel         matlab.ui.control.Label
        NicknameEditField               matlab.ui.control.EditField
        NicknameEditFieldLabel          matlab.ui.control.Label
        SelectButton                    matlab.ui.control.Button
        AddButton                       matlab.ui.control.Button
    end

    
    properties (Access = private)
        wind_load % wind load
        ice_load  % Ice load
        segurity_coeficient=0;
        zone
        theta_maxtension
        Cont_K            % constant K
        weight
        t_maximum         % (w) peso del conductor kg/m/mm²
        weigthconductor   % (P)Peso del conductor en kg/m
        
        % Generador de hipotesis
        theta_gen
        ice_load_gen=0;
        wind_load_gen=0;
        espesor_gen=0;
        gen_count=0; % Contador de hipotesis adicionales para subindice
        
        % Peor hipotesis
        m_desfavorable
        eye_desfavorable  % señal de posible hipotesis adicional
        
        % Generaracion basic graphic
        limit_graphic=0;
        sag_max % table to store the maximum sag state 
        sag_min % Table to store the minimum sag state
        account_graph=0;
    end
    
    methods (Access = private)
        
        % Zone definiton
        function zone_definition(app)
            % wind load definition based on diameter
            if app.DimetrommEditField.Value<16
                app.wind_load=60;
            else
                app.wind_load=50;
            end
            % ice load, temperature for maxumum tension hypotesis, and zone
            % definition
            if app.AlturamsnmmEditField.Value<500
                app.theta_maxtension=-5;
                app.ice_load=0;
                app.zone="A";
                app.ZonaCLamp_3.Color='green';
                app.ZonaCLamp_3.Enable='on';
                app.ZonaCLamp_3Label.Text='Zona A';
            elseif app.AlturamsnmmEditField.Value<1000
                app.theta_maxtension=-15;
                app.ice_load=(app.DimetrommEditField.Value)^(.5)*.18;
                app.zone="B";
                app.ZonaCLamp_3.Color='yellow';
                app.ZonaCLamp_3.Enable='on';
                app.ZonaCLamp_3Label.Text='Zona B';
            else
                app.theta_maxtension=-20;
                app.ice_load=(app.DimetrommEditField.Value)^(.5)*.36;
                app.zone="C";
                app.ZonaCLamp_3.Color='red';
                app.ZonaCLamp_3.Enable='on';
                app.ZonaCLamp_3Label.Text='Zona C';
            end
            % seguruty coefficient definition
            app.segurity_coeficient=app.SeguritycoefficientEditField.Value;
            app.weigthconductor=app.PesokgkmEditField.Value/1000;

        end
        
        % muximum tenstion hypotesis
        function maxtension_hyp(app)
            tc=app.CargaderupturakgEditField.Value/app.segurity_coeficient/app.SeccintransversalmmEditField.Value;
            app.t_maximum=tc;
            w=app.weigthconductor/app.SeccintransversalmmEditField.Value;
            app.weight=w;
            if app.zone=="A"
                hyp="A";
                Oveload_wind=app.wind_load;
                Overload_ice=0;
                app.theta_maxtension=-5;
                pc=sqrt(app.wind_load^2+app.weigthconductor^2);
                mc=pc/app.weigthconductor;
                app.Cont_K =tc-(app.VanomEditField.Value*mc*w)^2*app.ElasticidadEkgmmEditField.Value/(24*tc^2);
                fc=app.VanomEditField.Value^2*w*mc/(8*tc);
            elseif app.zone=="B"
                hyp="B";
                Oveload_wind=0;
                Overload_ice=app.ice_load;
                app.theta_maxtension=-15;
                pc=app.ice_load+app.weigthconductor;
                mc=pc/app.weigthconductor;
                app.Cont_K =tc-(app.VanomEditField.Value*mc*w)^2*app.ElasticidadEkgmmEditField.Value/(24*tc^2);
                fc=app.VanomEditField.Value^2*w*mc/(8*tc);
            else
                hyp="C";
                Oveload_wind=0;
                Overload_ice=app.ice_load;
                app.theta_maxtension=-20;
                pc=app.ice_load+app.weigthconductor;
                mc=pc/app.weigthconductor;
                app.Cont_K =tc-(app.VanomEditField.Value*mc*w)^2*app.ElasticidadEkgmmEditField.Value/(24*tc^2);
                fc=app.VanomEditField.Value^2*w*mc/(8*tc);
            end
            Tc=tc*app.SeccintransversalmmEditField.Value;
            coef_segurity=app.CargaderupturakgEditField.Value/Tc;
            t=table(hyp,app.theta_maxtension, Oveload_wind, Overload_ice, Tc, coef_segurity, mc, fc, NaN, "Tracción máxima",...
                'VariableNames' ,{'Hipotesis','Temperatura','Sobrecarga','SobrecargaM','Tension', 'security_coef','overload_coef','Flecha_vertical','Flecha_inclinada','Obervaciones'});
            app.UITable2.Data=t; 
            % additional hipotesis
            app.m_desfavorable=mc;
            app.UITable3_2.Data=t;
            app.eye_desfavorable=false;
        end
          
        % Change state equation        
        function [td, fd] = statechange(app,deltatheta,md)
            % Variables definition
            k=app.Cont_K;
            alfa=app.DilatacinEditField.Value;
            E=app.ElasticidadEkgmmEditField.Value;
            tc=app.t_maximum;
            a=app.VanomEditField.Value;
            w=app.weight;
            
            % tension
            td=fzero(@(x) x^2*(x-(k-alfa.*E.*deltatheta))-(a*w*md)^2*E/24, tc);
            
            %flecha
            fd=a^2*w*md/(8*td);
        end
        
        % Data generations for others hypotesis
        function otherhypotesis(app)
            % Wind hypotesis (d)
            % Computation
            theta=15;
            deltatheta=theta-app.theta_maxtension;
            pv=app.wind_load*app.DimetrommEditField.Value/1000;
            pd=sqrt(app.weigthconductor^2+pv^2);
            md=pd/app.weigthconductor;
            % Flecha y tension
            [td,fd]=statechange(app, deltatheta, md);
            Td=td*app.SeccintransversalmmEditField.Value;
            %Outputs
            hyp="D";
            Overload_wind=app.wind_load;
            Overload_ice=0;
            coef_segurity=app.CargaderupturakgEditField.Value/Td;
            t=table(hyp,theta,Overload_wind, Overload_ice, Td, coef_segurity, md,NaN, fd, "Flecha máxima",'VariableNames',...
                {'Hipotesis','Temperatura','Sobrecarga','SobrecargaM','Tension', 'security_coef','overload_coef','Flecha_vertical','Flecha_inclinada','Obervaciones'});
            app.UITable2.Data=union(t, app.UITable2.Data); 
            
            % Temperature hypotesis (e)
            theta=50;
            deltatheta=theta-app.theta_maxtension;
            me=1;
            [te,fe]=statechange(app, deltatheta, me);
            Te=te*app.SeccintransversalmmEditField.Value;
            %Outputs
            hyp="E";
            Overload_wind=0;
            Overload_ice=0;
            coef_segurity=app.CargaderupturakgEditField.Value/Te;
            t=table(hyp,theta,Overload_wind, Overload_ice, Te, coef_segurity, me, fe, NaN, "Flecha máxima",'VariableNames',...
                {'Hipotesis','Temperatura','Sobrecarga','SobrecargaM','Tension', 'security_coef','overload_coef','Flecha_vertical','Flecha_inclinada','Obervaciones'});
            app.UITable2.Data=union(t, app.UITable2.Data); 
            
            % Ice Hypotesis (f)
            theta=0;
            deltatheta=theta-app.theta_maxtension;
            pf=app.ice_load+app.weigthconductor;
            mf=pf/app.weigthconductor;
            [tf,ff]=statechange(app, deltatheta, mf);
            Tf=tf*app.SeccintransversalmmEditField.Value;
            %Outputs
            hyp="F";
            Overload_wind=0;
            Overload_ice=app.ice_load;
            coef_segurity=app.CargaderupturakgEditField.Value/Tf;
            t=table(hyp,theta,Overload_wind, Overload_ice, Tf, coef_segurity, mf, ff, NaN, "Flecha máxima",...
                'VariableNames' ,{'Hipotesis','Temperatura','Sobrecarga','SobrecargaM','Tension', 'security_coef','overload_coef','Flecha_vertical','Flecha_inclinada','Obervaciones'});
            app.UITable2.Data=union(t, app.UITable2.Data);
            
            % Estudios de fenomenos vibratorios (g)
            theta=15;
            deltatheta=theta-app.theta_maxtension;
            mg=1;
            [tg,fg]=statechange(app, deltatheta, mg);
            Tg=tg*app.SeccintransversalmmEditField.Value;
            TCD=Tg/app.CargaderupturakgEditField.Value*100;
            %Outputs
            hyp="G";
            Overload_wind=0;
            Overload_ice=0;
            coef_segurity=app.CargaderupturakgEditField.Value/Tg;
            text_aux=append("TCD=", num2str(TCD),"%" );
            t=table(hyp,theta,Overload_wind, Overload_ice, Tg, coef_segurity, mg, fg, NaN, text_aux,...
                'VariableNames' ,{'Hipotesis','Temperatura','Sobrecarga','SobrecargaM','Tension', 'security_coef','overload_coef','Flecha_vertical','Flecha_inclinada','Obervaciones'});
            app.UITable2.Data=union(t, app.UITable2.Data);
            
            % Tension en horas frias (h)
            theta=-5;
            deltatheta=theta-app.theta_maxtension;
            mh=1;
            [th,fh]=statechange(app, deltatheta, mh);
            Th=th*app.SeccintransversalmmEditField.Value;
            THF=Th/app.CargaderupturakgEditField.Value*100;
            %Outputs
            hyp="H";
            Overload_wind=0;
            Overload_ice=0;
            coef_segurity=app.CargaderupturakgEditField.Value/Th;
            text_aux=append("THF=", num2str(THF),"%" );
            t=table(hyp,theta,Overload_wind, Overload_ice, Th, coef_segurity, mh, fh, NaN, text_aux,...
                'VariableNames' ,{'Hipotesis','Temperatura','Sobrecarga','SobrecargaM','Tension', 'security_coef','overload_coef','Flecha_vertical','Flecha_inclinada','Obervaciones'});
            app.UITable2.Data=union(t, app.UITable2.Data);
            
            % Flecha minima (I)
            theta=-20;
            deltatheta=theta-app.theta_maxtension;
            mi=1;
            [ti,fi]=statechange(app, deltatheta, mi);
            Ti=ti*app.SeccintransversalmmEditField.Value;
            %Outputs
            hyp="I";
            Overload_wind=0;
            Overload_ice=0;
            coef_segurity=app.CargaderupturakgEditField.Value/Ti;
            t=table(hyp,theta,Overload_wind, Overload_ice, Ti, coef_segurity, mi, fi, NaN, "Flecha mínima",...
                'VariableNames' ,{'Hipotesis','Temperatura','Sobrecarga','SobrecargaM','Tension', 'security_coef','overload_coef','Flecha_vertical','Flecha_inclinada','Obervaciones'});
            app.UITable2.Data=union(t, app.UITable2.Data);
            
            % De viento (j)
            theta=-5;
            deltatheta=theta-app.theta_maxtension;
            mj=md;
            [tj,fj]=statechange(app, deltatheta, mj);
            Tj=tj*app.SeccintransversalmmEditField.Value;
            %Outputs
            hyp="J";
            Overload_wind=app.wind_load;
            Overload_ice=0;
            coef_segurity=app.CargaderupturakgEditField.Value/Tj;
            t=table(hyp,theta,Overload_wind, Overload_ice, Tj, coef_segurity, mj,NaN, fj, "-",'VariableNames' ,...
                {'Hipotesis','Temperatura','Sobrecarga','SobrecargaM','Tension', 'security_coef','overload_coef','Flecha_vertical','Flecha_inclinada','Obervaciones'});
            app.UITable2.Data=union(t, app.UITable2.Data);
            
        end
        
        % Ejecucion de hipotesis adicionales        
        function addhypotesis_data(app)
            app.theta_gen=app.TemperaturaCEditField.Value;
            if app.ActivarButton.Value==true
                if app.StandarCheckBox.Value==true
                    app.wind_load_gen=app.wind_load;
                elseif app.PresinCheckBox.Value==true
                    app.wind_load_gen=app.PresindevientokgmEditField.Value;
                else 
                    app.wind_load_gen=0.007*app.VelocidaddevientokmhEditField.Value^2*app.ReductioncoefficientEditField.Value;
                end
            else
               app.wind_load_gen=0; 
            end
            if app.ActivarButton_2.Value==true
                if app.StandarCheckBox_2.Value==true
                    app.ice_load_gen=app.ice_load;
                else
                    app.ice_load_gen=app.PesodelhielokgmEditField.Value;
                    app.espesor_gen=app.EspesormmEditField.Value;
                end
            else
                app.ice_load_gen=0;
                app.espesor_gen=0;
            end
            % additional hipotesis
            a=app.ActivarButton.Value==true;
            b=app.ActivarButton_2.Value==false;
            c=app.TemperaturaCEditField.Value==-15;
            d=app.TemperaturaCEditField.Value==-10;
            if app.zone=="B"
                if logical(a*b*d)
                    app.eye_desfavorable=true; 
                end
            end
            if app.zone=="C"
                if logical(a*b*c)
                    app.eye_desfavorable=true;  
                end
            end
        end
        
        % Hypotesis generation execution     
        function execution_gen(app)
            theta=app.theta_gen;
            deltatheta=theta-app.theta_maxtension;
            % load coefficient
            p=app.weigthconductor;
            ph=app.ice_load_gen;
            pv=app.wind_load_gen*(app.DimetrommEditField.Value/1000+2*app.espesor_gen/1000);
            disp(pv)
            pp=sqrt((p+ph)^2+pv^2);
            mj=pp/app.weigthconductor;
            % Tension and arrow
            [tj,fj]=statechange(app, deltatheta, mj);
            Tj=tj*app.SeccintransversalmmEditField.Value;
            %Outputs
            
            hyp=append("add", num2str(app.gen_count));
            Overload_wind=app.wind_load_gen;
            Overload_ice=app.ice_load_gen;
            coef_segurity=app.CargaderupturakgEditField.Value/Tj;
            if app.ActivarButton.Value==true
                t=table(hyp,theta,Overload_wind, Overload_ice, Tj, coef_segurity, mj,NaN, fj, "-",...
                    'VariableNames' ,{'Hipotesis','Temperatura','Sobrecarga','SobrecargaM','Tension', 'security_coef','overload_coef','Flecha_vertical','Flecha_inclinada','Obervaciones'});
            else
                t=table(hyp,theta,Overload_wind, Overload_ice, Tj, coef_segurity, mj, fj, NaN, "-",...
                    'VariableNames' ,{'Hipotesis','Temperatura','Sobrecarga','SobrecargaM','Tension', 'security_coef','overload_coef','Flecha_vertical','Flecha_inclinada','Obervaciones'});
            end
            app.UITable2.Data=union(t, app.UITable2.Data);
            
            % ejecucion de hipotesis adicional
            if app.eye_desfavorable
                if mj>app.m_desfavorable
                    app.UITable3_2.Data=t;
                    app.m_desfavorable=mj;
                end
                app.eye_desfavorable=false;
            end
        end
        
        % Load data in application       
        function load_data(app)
            if exist('data.xlsx','file')>0
                a='data.xlsx';
            else
                a=uigetfile;
            end
            t=readtable(a);
            app.UITable.Data=t;
            app.UITable.ColumnSortable=[true, false, false, false, false, false, false, false];
        end
        
        % Tabla de tendido    
        function tendido_table(app)
          app.UITable3.Data
          ArrayThetha=app.TemperaturamnimaminEditField.Value:app.IntervalosdetemperaturaSpinner.Value:app.TemperaturamximamaxEditField.Value;
          ArrayVano=app.VanomnimoEditField.Value:app.IntervaloentrevanosSpinner.Value:app.VanomximoEditField.Value;
          A=["Temperatura °C", "Tension [m]"];
          for k=1:length(ArrayVano)
              A(k+2)=append(num2str(ArrayVano(k)), " [m]");
          end
          B=[];
          for l=1:length(ArrayThetha)
            theta=ArrayThetha(l);
            deltatheta=theta-app.theta_maxtension;               
            [tj,fj]=statechange(app, deltatheta, 1);
            Tj=tj*app.SeccintransversalmmEditField.Value;
            flecha=fj.*(ArrayVano/app.VanomEditField.Value).^2;
            B(l,:)=[ArrayThetha(l), Tj, flecha];
          end
          Data=array2table(B);
          app.UITable3.Data=Data;
          app.UITable3.ColumnName=A;
          C=["k", "l"];
          for k=1:length(ArrayThetha)
              C(k)=num2str(k);
          end
          app.UITable3.RowName=C;
        end
        
%       Function to generate report
        function genReport(app)
        
            % Data verification
            if isempty(app.UITable3_2.Data)
                % Carga de datos por zona
                zone_definition(app);
                %Carga de la tabla de hipotesis
                maxtension_hyp(app);
                otherhypotesis(app);
                app.gen_count=1;
            end
            tendido_table(app)
            
            % import libraries
            import mlreportgen.report.*
            import mlreportgen.dom.*
            import mlreportgen.utils.*
            % Report Generation
            rpt = Report('myreport','pdf');
            add(rpt,TitlePage...
                ('Title','Cálculo de parámetros mecánicos de una línea de transmisión',...
            'Author','LIMECAL'))
            % Addition de Content Table
            add(rpt,TableOfContents)
            
            ch00 = Chapter('Title','Datos del Conductor');
            
            ch01 = Chapter('Title','Hipotesis');
            
            ch02 = Chapter('Title','Tabla de tendido');
            
            % Chapter 00 Datos del conductor
            
            ch00.Layout.Landscape=true;
            
            add(ch00,Text...
                ("Las siguientes líneas muestran los datos del caso de estudio:"));
            add(ch00,Text...
                (""));
            
            % Define cells
            if app.FaseCheckBox.Value==true
                tipo_cond="fase";
            else
                tipo_cond="Tierra";
            end
            
            Tnames=["Nombre","Diámetro [mm]", "Sección [mm²]", "Elasticidad [kg/mm²]", "Dilatación", "Peso [kg/km]", "Carga de Ruptura [kg]", "Tipo"];
            Tab_dat={app.NicknameEditField.Value,...
                num2str(round(app.DimetrommEditField.Value, 3)),...
                num2str(round(app.SeccintransversalmmEditField.Value, 3)),...
                num2str(round(app.ElasticidadEkgmmEditField.Value,3)),...
                num2str(round(app.DilatacinEditField.Value,6)),...
                num2str(round(app.PesokgkmEditField.Value,3)),...
                num2str(round(app.CargaderupturakgEditField.Value,3))...
                , tipo_cond};
            tbl_header0=array2table(Tnames);
            tbl_header=table2cell(tbl_header0);
            tend_table_data=Tab_dat;
            
            % Define formal table
            formalTable = mlreportgen.dom.FormalTable(tbl_header,tend_table_data);
            formalTable.RowSep = "Solid";
            formalTable.ColSep = "Solid";
            formalTable.Border = "Solid";
            formalTable.Header.TableEntriesStyle = [formalTable.Header.TableEntriesStyle,...
                {mlreportgen.dom.Bold(true)}];
            formalTable.TableEntriesStyle = [formalTable.TableEntriesStyle,...
                {mlreportgen.dom.InnerMargin("2pt","2pt","2pt","2pt"),...
                mlreportgen.dom.WhiteSpace("preserve")}];
            add(ch00, formalTable)
            
            add(ch00,Text...
                ("Las siguientes líneas muestran los datos adicionales del caso de estudio:"));
            add(ch00,Text...
                (""));
            
            Tnames=["Altura [m]", "Coeficiente de seguridad", "Vano [m]"];
            Tab_dat={app.AlturamsnmmEditField.Value, app.SeguritycoefficientEditField.Value, app.VanomEditField.Value};
            tbl_header0=array2table(Tnames);
            tbl_header=table2cell(tbl_header0);
            tend_table_data=Tab_dat;
            
            % Define formal table
            formalTable = mlreportgen.dom.FormalTable(tbl_header,tend_table_data);
            formalTable.RowSep = "Solid";
            formalTable.ColSep = "Solid";
            formalTable.Border = "Solid";
            formalTable.Header.TableEntriesStyle = [formalTable.Header.TableEntriesStyle,...
                {mlreportgen.dom.Bold(true)}];
            formalTable.TableEntriesStyle = [formalTable.TableEntriesStyle,...
                {mlreportgen.dom.InnerMargin("2pt","2pt","2pt","2pt"),...
                mlreportgen.dom.WhiteSpace("preserve")}];
            add(ch00, formalTable)
            
            add(rpt,ch00)
            
            % Chapter 01 Hipotesis -----------------------------------------
            % Change to LandScape
            
            ch01.Layout.Landscape=true;
            
            add(ch01,Text...
                ("Las siguientes líneas muestran el cálculo de las diferentes hipótesis:"));
            add(ch01,Text...
                (""));
            TabHyp=app.UITable2.Data;
                
            % first row
            Namesp=table("1Hipótesis", "Temperatura", "Carga Viento [kg/m]", "Carga Hielo [kg/m]", "Tensión [Kg]", "Coeficiente de seguridad",...
                "Coeficiente de sobrecarga", "Flecha vertica [m]", "Flecha inclinada [m]", "Observaciones", 'VariableNames', TabHyp.Properties.VariableNames);
            
            TabHyp.Temperatura=num2str(round(TabHyp.Temperatura,4));
            TabHyp.Sobrecarga=num2str(round(TabHyp.Sobrecarga,4));
            TabHyp.SobrecargaM=num2str(round(TabHyp.SobrecargaM,4));
            TabHyp.Tension=num2str(round(TabHyp.Tension,4));
            TabHyp.security_coef=num2str(round(TabHyp.security_coef,4));
            TabHyp.overload_coef=num2str(round(TabHyp.overload_coef,4));
            TabHyp.Flecha_vertical=num2str(round(TabHyp.Flecha_vertical,4));
            TabHyp.Flecha_inclinada=num2str(round(TabHyp.Flecha_inclinada,4));
            TabHyp=union(Namesp, TabHyp);
            % table to cell
            TabHyp2=table2cell(TabHyp);
            % Addition of table to report
            Tab01=BaseTable(TabHyp2);
            Tab01.Title='Tabla de hipótesis';
            add(ch01, Tab01)
            add(rpt,ch01)
            
            % Chapter 02 Tabla de tendido----------------------------- ***** *
            % worst hypotesis data
            
            ch02.Layout.Landscape=true;
            
            add(ch02,mlreportgen.dom.Text...
            ("Las siguientes líneas muestran la información de hipótesis más desfavorable: "))
            add(ch02,mlreportgen.dom.Text...
            (" "))
            
            TabHyp1=app.UITable3_2.Data;
            
            TabHyp1.Temperatura=num2str(round(TabHyp1.Temperatura,4));
            TabHyp1.Sobrecarga=num2str(round(TabHyp1.Sobrecarga,4));
            TabHyp1.SobrecargaM=num2str(round(TabHyp1.SobrecargaM,4));
            TabHyp1.Tension=num2str(round(TabHyp1.Tension,4));
            TabHyp1.security_coef=num2str(round(TabHyp1.security_coef,4));
            TabHyp1.overload_coef=num2str(round(TabHyp1.overload_coef,4));
            TabHyp1.Flecha_vertical=num2str(round(TabHyp1.Flecha_vertical,4));
            TabHyp1.Flecha_inclinada=num2str(round(TabHyp1.Flecha_inclinada,4));
            TabHyp1=union(Namesp, TabHyp1);
            
            TabHyp3=table2cell(TabHyp1);
            
            Tab02=BaseTable(TabHyp3);
            Tab02.Title='Tabla de la peor hipótesis';
            add(ch02, Tab02)
            
            % Tend table----------------------------------------------------
            
            add(ch02,mlreportgen.dom.Text...
                ("Las siguientes líneas muestran la tabla de tendido:"))
            add(ch02,mlreportgen.dom.Text...
                (" "))
            
            % define data
            TabHyp4=app.UITable3.Data;
            
            % Define header data
            ArrayVano=app.VanomnimoEditField.Value:app.IntervaloentrevanosSpinner.Value:app.VanomximoEditField.Value;
            A=["Temperatura °C", "Tension [kg]"];
            for k=1:length(ArrayVano)
                A(k+2)=append(num2str(ArrayVano(k)), " [m]");
            end
            
            % Body table defintion using 3 decimal
            tmp=[""];
            
            sz=size(TabHyp4);
            for k=1:sz(1)
                tmp(k,1)=[""];
            end
            for k=1:sz(2)
                tmp(:,k)=num2str(round(table2array(TabHyp4(:, k)),3));
            end
            tab_tend0=array2table(tmp);
           
            % Define cells
            tbl_header0=array2table(A);
            tbl_header=table2cell(tbl_header0);
            tend_table_data=table2cell(tab_tend0);
            
            % Define formal table
            formalTable = mlreportgen.dom.FormalTable(tbl_header,tend_table_data);
            formalTable.RowSep = "Solid";
            formalTable.ColSep = "Solid";
            formalTable.Border = "Solid";
            formalTable.Header.TableEntriesStyle = [formalTable.Header.TableEntriesStyle,...
                {mlreportgen.dom.Bold(true)}];
            formalTable.TableEntriesStyle = [formalTable.TableEntriesStyle,...
                {mlreportgen.dom.InnerMargin("2pt","2pt","2pt","2pt"),...
                mlreportgen.dom.WhiteSpace("preserve")}];
            
            % Generate several tables slicers    
            slicer = mlreportgen.utils.TableSlicer("Table",formalTable,"MaxCols",...
                12,"RepeatCols",1);
            slices = slicer.slice(); 
            
            for slice = slices
                str = sprintf("Repeated Column Index: %d ,SlicedColumns: From column %d to column %d",...
                    slicer.RepeatCols,slice.StartCol, slice.EndCol);
                para = Paragraph(str);
                para.Bold = true;
                para.Style = [para.Style,{KeepWithNext(true),...
                    OuterMargin("0pt","0pt","5pt","0pt")}];
                add(ch02,para);
                add(ch02,slice.Table);
            end
            
            % Adding chapter 2 to report
            add(rpt,ch02)   
            
            % Close Report 
            close(rpt)
            rptview(rpt)
            
        end
        
%       Catenaria Graph        
        function  BasicGraphic(app)
            % Variable definition
            a=app.VanomSpinner.Value;
            Tv=app.TensinvrticekgSpinner.Value;
            P=app.PesoaparentekgkmSpinner.Value/1000;
            h=Tv/P;
            % function definition
            Y=@(x) h.*cosh(x./h)-h.*cosh((a/2)./h); % Catenaria definition
            L=@(x) h.*sinh(x./h); % conductor length
            % plotting
            fplot(app.UIAxes, Y,[-a/2, a/2])
            % Definicicon de ejes
            if Y(0)>app.limit_graphic*0.75
                app.limit_graphic=Y(0);
            elseif Y(0)<app.limit_graphic*1.15
                app.limit_graphic=Y(0); 
            end
            axis(app.UIAxes, [-a*1.1/2, a*1.1/2, app.limit_graphic*1.15, -1*app.limit_graphic*0.05])
            % result fields
            app.FlechamEditField.Value=Y(0)*-1;
            app.LongituddeconductormEditField.Value=2*L(a/2);
        end
        
%       Flechas máximas y mínimas
        function max_and_min(app)
            % maximum sag
            % Variable definition
            a=app.VanomSpinner.Value;
            if app.UITable2.Data.Flecha_vertical(3)<app.UITable2.Data.Flecha_vertical(4)
                Tv=app.UITable2.Data.Tension(4);
                app.sag_max=app.UITable2.Data(4,:);
                P=app.PesokgkmEditField.Value/1000*app.UITable2.Data.overload_coef(4);
            else
                Tv=app.UITable2.Data.Tension(3);
                app.sag_max=app.UITable2.Data(3,:);
                P=app.PesokgkmEditField.Value/1000*app.UITable2.Data.overload_coef(3);
            end
            
            app.ParaunpesodeLabel.Text=append('Peso aparente: ', num2str(P));
            h=Tv/P;
            % function definition
            Y=@(x) h.*cosh(x./h)-h.*cosh((a/2)./h); % Catenaria definition
            L=@(x) h.*sinh(x./h); % conductor length
            % plotting
            hold(app.UIAxes, 'on')
            fplot(app.UIAxes, Y,[-a/2, a/2])
            % Definicicon de ejes
            if Y(0)>app.limit_graphic*0.75
                app.limit_graphic=Y(0);
            elseif Y(0)<app.limit_graphic*1.15
                app.limit_graphic=Y(0); 
            end
            axis(app.UIAxes, [-a*1.1/2, a*1.1/2, app.limit_graphic*1.15, -1*app.limit_graphic*0.05])
            % result fields
            app.FlechamximaLabel.Text =append('Flecha máxima: ',num2str(Y(0)*-1));
            app.PesoaparenteLabel_2.Text=append('Longitud máxima: ',num2str(2*L(a/2)));
            
            % minimum sag
            % Variable definition
            a=app.VanomSpinner.Value;
            Tv=app.UITable2.Data.Tension(7) ;
            P=app.PesokgkmEditField.Value/1000*app.UITable2.Data.overload_coef(7);
            app.PesoaparenteLabel.Text=append('Peso aparente: ', num2str(P));
            h=Tv/P;
            % function definition
            Y=@(x) h.*cosh(x./h)-h.*cosh((a/2)./h); % Catenaria definition
            L=@(x) h.*sinh(x./h); % conductor length
            % plotting
            fplot(app.UIAxes, Y,[-a/2, a/2])
            
            % result fields
            app.FlechamnimaLabel.Text =append('Flecha mínima: ',num2str(Y(0)*-1));
            app.LongitudmnimaLabel_2.Text=append('Longitud mínima: ',num2str(2*L(a/2)));
            hold(app.UIAxes, 'off')
        end
    end
  

  

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app)
            load_data(app)
            app.UITable_2.Data=app.UITable.Data(1,:);
            
        end

        % Button pushed function: AddButton
        function AddButtonPushed(app, event)
            % Carga de datos por zona
            zone_definition(app);
            %Carga de la tabla de hipotesis
            maxtension_hyp(app);
            otherhypotesis(app);
            app.gen_count=1;
        end

        % Button pushed function: AadirhiptesisButton
        function AadirhiptesisButtonPushed(app, event)
            app.AadirhiptesisButton.Enable='off';
            if app.gen_count==0
                % Carga de datos por zona
                zone_definition(app);
                %Carga de la tabla de hipotesis
                maxtension_hyp(app);
                otherhypotesis(app);
                app.gen_count=1;
            end
            addhypotesis_data(app)
            execution_gen(app)
            app.gen_count=app.gen_count+1;
            app.AadirhiptesisButton.Enable='on';
        end

        % Value changed function: ActivarButton
        function ActivarButtonValueChanged(app, event)
            value = app.ActivarButton.Value;
            if value==true
                app.StandarCheckBox.Enable='on';
                app.PresinCheckBox.Enable='on';
                app.VelocidadCheckBox.Enable='on';
                if app.PresinCheckBox.Value==true
                    app.PresindevientokgmEditField.Enable='on';
                elseif app.VelocidadCheckBox.Value==true
                    app.VelocidaddevientokmhEditField.Enable='on';
                    app.ReductioncoefficientEditField.Enable='on';
                end
            else
                app.StandarCheckBox.Enable='off';
                app.PresinCheckBox.Enable='off';
                app.VelocidadCheckBox.Enable='off';
                app.PresindevientokgmEditField.Enable='off';
                app.VelocidaddevientokmhEditField.Enable='off';
                app.ReductioncoefficientEditField.Enable='off';
            end
        end

        % Value changed function: ActivarButton_2
        function ActivarButton_2ValueChanged(app, event)
            value = app.ActivarButton_2.Value;
            if value==true
                app.StandarCheckBox_2.Enable='on';
                app.PesoCheckBox.Enable='on';
                if app.PesoCheckBox.Value==true
                    app.PesodelhielokgmEditField.Enable='on';
                    app.EspesormmEditField.Enable='on';
                end
            else
                app.StandarCheckBox_2.Enable='off';
                app.PesoCheckBox.Enable='off';
                app.PesodelhielokgmEditField.Enable='off';
                app.EspesormmEditField.Enable='off';
                
            end
        end

        % Value changed function: PresinCheckBox
        function PresinCheckBoxValueChanged(app, event)
            value = app.PresinCheckBox.Value;
            if value ==true
                app.StandarCheckBox.Value=false;
                app.VelocidadCheckBox.Value=false;
                app.PresindevientokgmEditField.Enable='on';
                app.VelocidaddevientokmhEditField.Enable='off';
                app.ReductioncoefficientEditField.Enable='off';
            else
                app.PresinCheckBox.Value=true;
            end
        end

        % Value changed function: StandarCheckBox
        function StandarCheckBoxValueChanged(app, event)
            value = app.StandarCheckBox.Value;
            if value ==true
                app.PresinCheckBox.Value=false;
                app.VelocidadCheckBox.Value=false;
                app.PresindevientokgmEditField.Enable='off';
                app.VelocidaddevientokmhEditField.Enable='off';
                app.ReductioncoefficientEditField.Enable='off';
            else
                app.StandarCheckBox.Value=true;
            end
        end

        % Value changed function: VelocidadCheckBox
        function VelocidadCheckBoxValueChanged(app, event)
            value = app.VelocidadCheckBox.Value;
            if value ==true
                app.PresinCheckBox.Value=false;
                app.StandarCheckBox.Value=false;
                app.PresindevientokgmEditField.Enable='off';
                app.VelocidaddevientokmhEditField.Enable='on';
                app.ReductioncoefficientEditField.Enable='on';
            else
                app.VelocidadCheckBox.Value=true;
            end
        end

        % Value changed function: PesoCheckBox
        function PesoCheckBoxValueChanged(app, event)
            value = app.PesoCheckBox.Value;
            if value==true
                app.StandarCheckBox_2.Value=false;
                app.PesodelhielokgmEditField.Enable='on';
                app.EspesormmEditField.Enable='on';
            else
                app.PesoCheckBox.Value=true;
            end
        end

        % Value changed function: StandarCheckBox_2
        function StandarCheckBox_2ValueChanged(app, event)
            value = app.StandarCheckBox_2.Value;
            if value==true
                app.PesoCheckBox.Value=false;
                app.PesodelhielokgmEditField.Enable='off';
                app.EspesormmEditField.Enable='off';
            else
                app.StandarCheckBox_2.Value=true;
            end
        end

        % Cell selection callback: UITable
        function UITableCellSelection(app, event)
            indices = event.Indices;
            if indices(2)==1
                app.UITable_2.Data=app.UITable.Data(indices(1),:);
            end
        end

        % Button pushed function: CalcularcasodeestudioButton
        function CalcularcasodeestudioButtonPushed(app, event)
            t=app.UITable_2.Data;
            app.NicknameEditField.Value=t.Nickname{1};
            app.DimetrommEditField.Value=t.Di_metroDelCable_mm_;
            app.SeccintransversalmmEditField.Value=t.SeccionesTransveersalTotal_mm__;
            app.ElasticidadEkgmmEditField.Value=t.M_duloDeElasticidad_kg_mm__;
            app.DilatacinEditField.Value=t.CoeficienteDeDilataci_n;
            app.PesokgkmEditField.Value=t.PesosTotal_kg_km_;
            app.CargaderupturakgEditField.Value=t.CargaDeRotura_kg_;
            if size(t.Tipo{1})==size('fase')
                app.FaseCheckBox.Value=true;
                app.CabledetierraCheckBox.Value=false;
            else
                app.FaseCheckBox.Value=false;
                app.CabledetierraCheckBox.Value=true;
            end
            % Carga de datos por zona
            zone_definition(app);
            %Carga de la tabla de hipotesis
            maxtension_hyp(app);
            otherhypotesis(app);
            app.gen_count=1;
        end

        % Cell selection callback: UITable2
        function UITable2CellSelection(app, event)
            indices = event.Indices;
            if indices(2)==1
                
            end
        end

        % Value changed function: FaseCheckBox
        function FaseCheckBoxValueChanged(app, event)
            value = app.FaseCheckBox.Value;
            if value==true
                app.CabledetierraCheckBox.Value=false;
            else
                app.FaseCheckBox.Value=true;
            end
        end

        % Value changed function: CabledetierraCheckBox
        function CabledetierraCheckBoxValueChanged(app, event)
            value = app.CabledetierraCheckBox.Value;
            if value==true
                app.FaseCheckBox.Value=false;
            else
                app.CabledetierraCheckBox.Value=true;
            end
        end

        % Button pushed function: SelectButton
        function SelectButtonPushed(app, event)
            app.TabGroup.SelectedTab=app.InputTab;
            
        end

        % Button pushed function: GenerartablaButton
        function GenerartablaButtonPushed(app, event)
            app.GenerartablaButton.Enable='off';
            if isempty(app.UITable3_2.Data)
                % Carga de datos por zona
                zone_definition(app);
                %Carga de la tabla de hipotesis
                maxtension_hyp(app);
                otherhypotesis(app);
                app.gen_count=1;
            end
            tendido_table(app)
            app.GenerartablaButton.Enable='on';
        end

        % Value changed function: TemperaturamnimaminEditField
        function TemperaturamnimaminEditFieldValueChanged(app, event)
            value = app.TemperaturamnimaminEditField.Value;
            if value>app.TemperaturamximamaxEditField.Value
                app.TemperaturamnimaminEditField.Value=app.TemperaturamximamaxEditField.Value;
            end
        end

        % Value changed function: TemperaturamximamaxEditField
        function TemperaturamximamaxEditFieldValueChanged(app, event)
            value = app.TemperaturamximamaxEditField.Value;
            if value<app.TemperaturamnimaminEditField.Value
                app.TemperaturamximamaxEditField.Value=app.TemperaturamnimaminEditField.Value;
            end
        end

        % Value changed function: VanomnimoEditField
        function VanomnimoEditFieldValueChanged(app, event)
            value = app.VanomnimoEditField.Value;
            if value>app.VanomximoEditField.Value
                app.VanomnimoEditField.Value=app.VanomximoEditField.Value;
            end
        end

        % Value changed function: VanomximoEditField
        function VanomximoEditFieldValueChanged(app, event)
            value = app.VanomximoEditField.Value;
            if value<app.VanomnimoEditField.Value
                app.VanomximoEditField.Value=app.VanomnimoEditField.Value;
            end
        end

        % Button pushed function: LoadDataButton
        function LoadDataButtonPushed(app, event)
            app.LoadDataButton.Enable='off';
            k="";
            monty=0;
            a=1;
            while (((k~='.xlsx') ||(k~='.xls'))&&(monty~=1))||(a~=0)
                a=uigetfile(...
                'Excel Files (*.xlsx, *.xls)',...
                'Select a file');
                k=a(end-3:end);
                t=readtable(a);
                namesV=t.Properties.VariableNames;
                first_name='Nickname'==namesV{1};
                b=size(t);
                size_compo=b(2)==8;
                if logical(first_name*size_compo)
                    monty=1;
                end
            end
            app.UITable.Data=t;
            app.UITable.ColumnSortable=[true, false, false, false, false, false, false, false];
            
            app.LoadDataButton.Enable='on';
            
        end

        % Menu selected function: PdfMenu
        function PdfMenuSelected(app, event)
            genReport(app)
        end

        % Button pushed function: AplicarButton
        function AplicarButtonPushed(app, event)
            % Data verification
            if isempty(app.UITable3_2.Data)
                % Carga de datos por zona
                zone_definition(app);
                %Carga de la tabla de hipotesis
                maxtension_hyp(app);
                otherhypotesis(app);
                app.gen_count=1;
            end
            
            % Registration of data in spinners
            app.VanomSpinner.Value=app.VanomEditField.Value;
            app.TensinvrticekgSpinner.Value=app.UITable3_2.Data.Tension;
            app.PesoaparentekgkmSpinner.Value=app.PesokgkmEditField.Value.*app.UITable3_2.Data.overload_coef;
            % Actualizacion de graficos
            BasicGraphic(app)
            if app.FlechasmximasymnimasCheckBox.Value==true
                max_and_min(app)
            end
            
        end

        % Value changing function: PesoaparentekgkmSpinner
        function PesoaparentekgkmSpinnerValueChanging(app, event)
            changingValue = event.Value;
            if ischar(changingValue)
                app.PesoaparentekgkmSpinner.Value=str2double(changingValue);
            else
                app.PesoaparentekgkmSpinner.Value=changingValue;
            end
            
            BasicGraphic(app)
            if app.FlechasmximasymnimasCheckBox.Value==true
                max_and_min(app)
            end
        end

        % Value changing function: TensinvrticekgSpinner
        function TensinvrticekgSpinnerValueChanging(app, event)
            changingValue = event.Value;
            if ischar(changingValue)
                app.TensinvrticekgSpinner.Value=str2double(changingValue);
            else
                app.TensinvrticekgSpinner.Value=changingValue;
            end
            BasicGraphic(app)
            if app.FlechasmximasymnimasCheckBox.Value==true
                max_and_min(app)
            end
        end

        % Value changing function: VanomSpinner
        function VanomSpinnerValueChanging(app, event)
            changingValue = event.Value;
            if ischar(changingValue)
                app.VanomSpinner.Value=str2double(changingValue);
            else
                app.VanomSpinner.Value=changingValue;
            end
            BasicGraphic(app)
            if app.FlechasmximasymnimasCheckBox.Value==true
                max_and_min(app)
            end
        end

        % Value changed function: FlechasmximasymnimasCheckBox
        function FlechasmximasymnimasCheckBoxValueChanged(app, event)
            value = app.FlechasmximasymnimasCheckBox.Value;
            
            % Data verification
            if isempty(app.UITable3_2.Data)
                % Carga de datos por zona
                zone_definition(app);
                %Carga de la tabla de hipotesis
                maxtension_hyp(app);
                otherhypotesis(app);
                app.gen_count=1;
            end
            if app.account_graph==0
                % Registration of data in spinners
                app.VanomSpinner.Value=app.VanomEditField.Value;
                app.TensinvrticekgSpinner.Value=app.UITable3_2.Data.Tension;
                app.PesoaparentekgkmSpinner.Value=app.PesokgkmEditField.Value.*app.UITable3_2.Data.overload_coef;
            end
            
            if value==true
                max_and_min(app)
            else
                BasicGraphic(app)
            end

        end

        % Menu selected function: ReferenciaMenu
        function ReferenciaMenuSelected(app, event)
            msgbox('Basado en el libro: Líneas de transporte de energía. por el Dr. Luis María Checa 3 edición', 'Referencia');
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 1428 746];
            app.UIFigure.Name = 'UI Figure';

            % Create ArchivoMenu
            app.ArchivoMenu = uimenu(app.UIFigure);
            app.ArchivoMenu.Text = 'Archivo';

            % Create GenerarReporteMenu
            app.GenerarReporteMenu = uimenu(app.ArchivoMenu);
            app.GenerarReporteMenu.Text = 'Generar Reporte';

            % Create PdfMenu
            app.PdfMenu = uimenu(app.GenerarReporteMenu);
            app.PdfMenu.MenuSelectedFcn = createCallbackFcn(app, @PdfMenuSelected, true);
            app.PdfMenu.Text = 'Pdf';

            % Create ReferenciaMenu
            app.ReferenciaMenu = uimenu(app.UIFigure);
            app.ReferenciaMenu.MenuSelectedFcn = createCallbackFcn(app, @ReferenciaMenuSelected, true);
            app.ReferenciaMenu.Text = 'Referencia';

            % Create InformacindelcasodeestudioPanel
            app.InformacindelcasodeestudioPanel = uipanel(app.UIFigure);
            app.InformacindelcasodeestudioPanel.Title = 'Información del caso de estudio';
            app.InformacindelcasodeestudioPanel.FontWeight = 'bold';
            app.InformacindelcasodeestudioPanel.Position = [4 7 260 736];

            % Create AddButton
            app.AddButton = uibutton(app.InformacindelcasodeestudioPanel, 'push');
            app.AddButton.ButtonPushedFcn = createCallbackFcn(app, @AddButtonPushed, true);
            app.AddButton.Position = [21 110 100 22];
            app.AddButton.Text = 'Add';

            % Create SelectButton
            app.SelectButton = uibutton(app.InformacindelcasodeestudioPanel, 'push');
            app.SelectButton.ButtonPushedFcn = createCallbackFcn(app, @SelectButtonPushed, true);
            app.SelectButton.Position = [141 110 100 22];
            app.SelectButton.Text = 'Select';

            % Create NicknameEditFieldLabel
            app.NicknameEditFieldLabel = uilabel(app.InformacindelcasodeestudioPanel);
            app.NicknameEditFieldLabel.HorizontalAlignment = 'right';
            app.NicknameEditFieldLabel.Position = [31 653 59 22];
            app.NicknameEditFieldLabel.Text = 'Nickname';

            % Create NicknameEditField
            app.NicknameEditField = uieditfield(app.InformacindelcasodeestudioPanel, 'text');
            app.NicknameEditField.Position = [125 653 106 22];
            app.NicknameEditField.Value = 'Halcon';

            % Create DimetrommEditFieldLabel
            app.DimetrommEditFieldLabel = uilabel(app.InformacindelcasodeestudioPanel);
            app.DimetrommEditFieldLabel.HorizontalAlignment = 'right';
            app.DimetrommEditFieldLabel.Position = [24 605 84 22];
            app.DimetrommEditFieldLabel.Text = 'Diámetro [mm]';

            % Create DimetrommEditField
            app.DimetrommEditField = uieditfield(app.InformacindelcasodeestudioPanel, 'numeric');
            app.DimetrommEditField.Limits = [0 Inf];
            app.DimetrommEditField.Position = [125 605 106 22];
            app.DimetrommEditField.Value = 21.793;

            % Create SeccintransversalmmLabel
            app.SeccintransversalmmLabel = uilabel(app.InformacindelcasodeestudioPanel);
            app.SeccintransversalmmLabel.HorizontalAlignment = 'center';
            app.SeccintransversalmmLabel.Position = [27.5 548 98 28];
            app.SeccintransversalmmLabel.Text = {'Sección '; 'transversal [mm²]'};

            % Create SeccintransversalmmEditField
            app.SeccintransversalmmEditField = uieditfield(app.InformacindelcasodeestudioPanel, 'numeric');
            app.SeccintransversalmmEditField.Limits = [0 Inf];
            app.SeccintransversalmmEditField.Position = [125 554 106 22];
            app.SeccintransversalmmEditField.Value = 281.1;

            % Create ElasticidadEkgmmLabel
            app.ElasticidadEkgmmLabel = uilabel(app.InformacindelcasodeestudioPanel);
            app.ElasticidadEkgmmLabel.HorizontalAlignment = 'center';
            app.ElasticidadEkgmmLabel.Position = [31 496 90 40];
            app.ElasticidadEkgmmLabel.Text = {'Elasticidad E'; '[kg/mm²]'};

            % Create ElasticidadEkgmmEditField
            app.ElasticidadEkgmmEditField = uieditfield(app.InformacindelcasodeestudioPanel, 'numeric');
            app.ElasticidadEkgmmEditField.Limits = [0 Inf];
            app.ElasticidadEkgmmEditField.Position = [125 504 106 22];
            app.ElasticidadEkgmmEditField.Value = 7730;

            % Create DilatacinEditFieldLabel
            app.DilatacinEditFieldLabel = uilabel(app.InformacindelcasodeestudioPanel);
            app.DilatacinEditFieldLabel.HorizontalAlignment = 'center';
            app.DilatacinEditFieldLabel.Position = [31 464 68 22];
            app.DilatacinEditFieldLabel.Text = 'Dilatación α';

            % Create DilatacinEditField
            app.DilatacinEditField = uieditfield(app.InformacindelcasodeestudioPanel, 'numeric');
            app.DilatacinEditField.Limits = [0 Inf];
            app.DilatacinEditField.Position = [125 464 106 22];
            app.DilatacinEditField.Value = 1.899e-05;

            % Create PesokgkmEditFieldLabel
            app.PesokgkmEditFieldLabel = uilabel(app.InformacindelcasodeestudioPanel);
            app.PesokgkmEditFieldLabel.HorizontalAlignment = 'center';
            app.PesokgkmEditFieldLabel.Position = [27 406 80 30];
            app.PesokgkmEditFieldLabel.Text = {'Peso  '; '[kg/km]'};

            % Create PesokgkmEditField
            app.PesokgkmEditField = uieditfield(app.InformacindelcasodeestudioPanel, 'numeric');
            app.PesokgkmEditField.Limits = [0 Inf];
            app.PesokgkmEditField.Position = [125 414 106 22];
            app.PesokgkmEditField.Value = 974.6;

            % Create CargaderupturakgLabel
            app.CargaderupturakgLabel = uilabel(app.InformacindelcasodeestudioPanel);
            app.CargaderupturakgLabel.HorizontalAlignment = 'center';
            app.CargaderupturakgLabel.Position = [31 350 90 37];
            app.CargaderupturakgLabel.Text = {'Carga de'; ' ruptura [kg]'};

            % Create CargaderupturakgEditField
            app.CargaderupturakgEditField = uieditfield(app.InformacindelcasodeestudioPanel, 'numeric');
            app.CargaderupturakgEditField.Limits = [0 Inf];
            app.CargaderupturakgEditField.Position = [125 365 106 22];
            app.CargaderupturakgEditField.Value = 8817.8;

            % Create DatosdelconductorLabel
            app.DatosdelconductorLabel = uilabel(app.InformacindelcasodeestudioPanel);
            app.DatosdelconductorLabel.FontWeight = 'bold';
            app.DatosdelconductorLabel.Position = [21 683 122 22];
            app.DatosdelconductorLabel.Text = 'Datos del conductor';

            % Create DatosadicionalesLabel
            app.DatosadicionalesLabel = uilabel(app.InformacindelcasodeestudioPanel);
            app.DatosadicionalesLabel.FontWeight = 'bold';
            app.DatosadicionalesLabel.Position = [21 269 107 22];
            app.DatosadicionalesLabel.Text = 'Datos adicionales';

            % Create AlturamsnmmEditFieldLabel
            app.AlturamsnmmEditFieldLabel = uilabel(app.InformacindelcasodeestudioPanel);
            app.AlturamsnmmEditFieldLabel.HorizontalAlignment = 'center';
            app.AlturamsnmmEditFieldLabel.Position = [18 229 92 22];
            app.AlturamsnmmEditFieldLabel.Text = 'Altura msnm [m]';

            % Create AlturamsnmmEditField
            app.AlturamsnmmEditField = uieditfield(app.InformacindelcasodeestudioPanel, 'numeric');
            app.AlturamsnmmEditField.Limits = [0 Inf];
            app.AlturamsnmmEditField.Position = [127 229 100 22];
            app.AlturamsnmmEditField.Value = 1000;

            % Create SeguritycoefficientEditFieldLabel
            app.SeguritycoefficientEditFieldLabel = uilabel(app.InformacindelcasodeestudioPanel);
            app.SeguritycoefficientEditFieldLabel.HorizontalAlignment = 'right';
            app.SeguritycoefficientEditFieldLabel.Position = [6 190 106 22];
            app.SeguritycoefficientEditFieldLabel.Text = 'Segurity coefficient';

            % Create SeguritycoefficientEditField
            app.SeguritycoefficientEditField = uieditfield(app.InformacindelcasodeestudioPanel, 'numeric');
            app.SeguritycoefficientEditField.Limits = [0 10];
            app.SeguritycoefficientEditField.Position = [127 190 100 22];
            app.SeguritycoefficientEditField.Value = 3;

            % Create VanomEditFieldLabel
            app.VanomEditFieldLabel = uilabel(app.InformacindelcasodeestudioPanel);
            app.VanomEditFieldLabel.HorizontalAlignment = 'right';
            app.VanomEditFieldLabel.Position = [59 150 52 22];
            app.VanomEditFieldLabel.Text = 'Vano [m]';

            % Create VanomEditField
            app.VanomEditField = uieditfield(app.InformacindelcasodeestudioPanel, 'numeric');
            app.VanomEditField.Limits = [0 Inf];
            app.VanomEditField.Position = [126 150 100 22];
            app.VanomEditField.Value = 300;

            % Create FaseCheckBox
            app.FaseCheckBox = uicheckbox(app.InformacindelcasodeestudioPanel);
            app.FaseCheckBox.ValueChangedFcn = createCallbackFcn(app, @FaseCheckBoxValueChanged, true);
            app.FaseCheckBox.Text = 'Fase';
            app.FaseCheckBox.Position = [64 308 48 22];
            app.FaseCheckBox.Value = true;

            % Create CabledetierraCheckBox
            app.CabledetierraCheckBox = uicheckbox(app.InformacindelcasodeestudioPanel);
            app.CabledetierraCheckBox.ValueChangedFcn = createCallbackFcn(app, @CabledetierraCheckBoxValueChanged, true);
            app.CabledetierraCheckBox.Text = {'Cable de'; 'tierra'};
            app.CabledetierraCheckBox.Position = [154 305 70 28];

            % Create ZonaCLamp_3Label
            app.ZonaCLamp_3Label = uilabel(app.InformacindelcasodeestudioPanel);
            app.ZonaCLamp_3Label.HorizontalAlignment = 'right';
            app.ZonaCLamp_3Label.FontSize = 9;
            app.ZonaCLamp_3Label.Position = [24 49 35 22];
            app.ZonaCLamp_3Label.Text = 'Zona C';

            % Create ZonaCLamp_3
            app.ZonaCLamp_3 = uilamp(app.InformacindelcasodeestudioPanel);
            app.ZonaCLamp_3.Position = [64 55 10 10];
            app.ZonaCLamp_3.Color = [1 0.4118 0.1608];

            % Create ByJimmyPalominoLabel
            app.ByJimmyPalominoLabel = uilabel(app.InformacindelcasodeestudioPanel);
            app.ByJimmyPalominoLabel.Position = [32 8 111 22];
            app.ByJimmyPalominoLabel.Text = 'By: Jimmy Palomino';

            % Create Hyperlink
            app.Hyperlink = uihyperlink(app.InformacindelcasodeestudioPanel);
            app.Hyperlink.URL = 'https://github.com/JimsPalo';
            app.Hyperlink.Position = [147 8 54 22];
            app.Hyperlink.Text = 'LinkedIn';

            % Create TabGroup
            app.TabGroup = uitabgroup(app.UIFigure);
            app.TabGroup.Position = [271 7 1151 734];

            % Create HiptesisTab
            app.HiptesisTab = uitab(app.TabGroup);
            app.HiptesisTab.Title = 'Hipótesis';

            % Create UITable2
            app.UITable2 = uitable(app.HiptesisTab);
            app.UITable2.ColumnName = {'Hipótesis'; 'Temperatura °C'; 'Carga Viento [kg/m]'; 'Carga Hielo [kg/m]'; 'Tensión [m]'; 'Segurity coef'; 'overload coef'; 'Flecha vertical [m]'; 'Flecha inclinada [m]'; 'Obervaciones'};
            app.UITable2.RowName = {};
            app.UITable2.CellSelectionCallback = createCallbackFcn(app, @UITable2CellSelection, true);
            app.UITable2.Position = [60 39 1041 439];

            % Create AadirhiptesisButton
            app.AadirhiptesisButton = uibutton(app.HiptesisTab, 'push');
            app.AadirhiptesisButton.ButtonPushedFcn = createCallbackFcn(app, @AadirhiptesisButtonPushed, true);
            app.AadirhiptesisButton.Position = [981 486 100 22];
            app.AadirhiptesisButton.Text = 'Añadir hipótesis';

            % Create Generadordehipotesis1Label
            app.Generadordehipotesis1Label = uilabel(app.HiptesisTab);
            app.Generadordehipotesis1Label.FontWeight = 'bold';
            app.Generadordehipotesis1Label.Position = [31 680 150 22];
            app.Generadordehipotesis1Label.Text = 'Generador de hipotesis 1';

            % Create VientoPanel
            app.VientoPanel = uipanel(app.HiptesisTab);
            app.VientoPanel.Title = 'Viento';
            app.VientoPanel.FontWeight = 'bold';
            app.VientoPanel.Position = [415 519 330 153];

            % Create PresindevientokgmEditFieldLabel
            app.PresindevientokgmEditFieldLabel = uilabel(app.VientoPanel);
            app.PresindevientokgmEditFieldLabel.HorizontalAlignment = 'center';
            app.PresindevientokgmEditFieldLabel.Position = [17 69 101 28];
            app.PresindevientokgmEditFieldLabel.Text = {'Presión de viento '; '[kg/m²]'};

            % Create PresindevientokgmEditField
            app.PresindevientokgmEditField = uieditfield(app.VientoPanel, 'numeric');
            app.PresindevientokgmEditField.Limits = [0 Inf];
            app.PresindevientokgmEditField.Enable = 'off';
            app.PresindevientokgmEditField.Position = [133 73 100 22];
            app.PresindevientokgmEditField.Value = 50.4;

            % Create VelocidaddevientokmhLabel
            app.VelocidaddevientokmhLabel = uilabel(app.VientoPanel);
            app.VelocidaddevientokmhLabel.HorizontalAlignment = 'center';
            app.VelocidaddevientokmhLabel.Position = [6 34 113 28];
            app.VelocidaddevientokmhLabel.Text = {'Velocidad de viento '; '[km/h]'};

            % Create VelocidaddevientokmhEditField
            app.VelocidaddevientokmhEditField = uieditfield(app.VientoPanel, 'numeric');
            app.VelocidaddevientokmhEditField.Limits = [0 Inf];
            app.VelocidaddevientokmhEditField.Enable = 'off';
            app.VelocidaddevientokmhEditField.Position = [134 40 100 22];
            app.VelocidaddevientokmhEditField.Value = 120;

            % Create PresinCheckBox
            app.PresinCheckBox = uicheckbox(app.VientoPanel);
            app.PresinCheckBox.ValueChangedFcn = createCallbackFcn(app, @PresinCheckBoxValueChanged, true);
            app.PresinCheckBox.Enable = 'off';
            app.PresinCheckBox.Text = 'Presión';
            app.PresinCheckBox.Position = [250 74 62 22];

            % Create VelocidadCheckBox
            app.VelocidadCheckBox = uicheckbox(app.VientoPanel);
            app.VelocidadCheckBox.ValueChangedFcn = createCallbackFcn(app, @VelocidadCheckBoxValueChanged, true);
            app.VelocidadCheckBox.Enable = 'off';
            app.VelocidadCheckBox.Text = 'Velocidad';
            app.VelocidadCheckBox.Position = [250 39 74 22];

            % Create StandarCheckBox
            app.StandarCheckBox = uicheckbox(app.VientoPanel);
            app.StandarCheckBox.ValueChangedFcn = createCallbackFcn(app, @StandarCheckBoxValueChanged, true);
            app.StandarCheckBox.Enable = 'off';
            app.StandarCheckBox.Text = 'Standar';
            app.StandarCheckBox.Position = [251 105 64 22];
            app.StandarCheckBox.Value = true;

            % Create ActivarButton
            app.ActivarButton = uibutton(app.VientoPanel, 'state');
            app.ActivarButton.ValueChangedFcn = createCallbackFcn(app, @ActivarButtonValueChanged, true);
            app.ActivarButton.Text = 'Activar';
            app.ActivarButton.Position = [21 105 100 22];

            % Create ReductioncoefficientEditFieldLabel
            app.ReductioncoefficientEditFieldLabel = uilabel(app.VientoPanel);
            app.ReductioncoefficientEditFieldLabel.HorizontalAlignment = 'center';
            app.ReductioncoefficientEditFieldLabel.Position = [15 9 117 22];
            app.ReductioncoefficientEditFieldLabel.Text = 'Reduction coefficient';

            % Create ReductioncoefficientEditField
            app.ReductioncoefficientEditField = uieditfield(app.VientoPanel, 'numeric');
            app.ReductioncoefficientEditField.Limits = [0 Inf];
            app.ReductioncoefficientEditField.Enable = 'off';
            app.ReductioncoefficientEditField.Position = [131 9 100 22];
            app.ReductioncoefficientEditField.Value = 0.5;

            % Create HieloPanel
            app.HieloPanel = uipanel(app.HiptesisTab);
            app.HieloPanel.Title = 'Hielo';
            app.HieloPanel.FontWeight = 'bold';
            app.HieloPanel.Position = [772 519 330 153];

            % Create PesodelhielokgmLabel
            app.PesodelhielokgmLabel = uilabel(app.HieloPanel);
            app.PesodelhielokgmLabel.HorizontalAlignment = 'center';
            app.PesodelhielokgmLabel.Position = [10 47 81 28];
            app.PesodelhielokgmLabel.Text = {'Peso del hielo'; ' [kg/m]'};

            % Create PesodelhielokgmEditField
            app.PesodelhielokgmEditField = uieditfield(app.HieloPanel, 'numeric');
            app.PesodelhielokgmEditField.Limits = [0 Inf];
            app.PesodelhielokgmEditField.Enable = 'off';
            app.PesodelhielokgmEditField.Position = [106 53 100 22];
            app.PesodelhielokgmEditField.Value = 1.6806;

            % Create StandarCheckBox_2
            app.StandarCheckBox_2 = uicheckbox(app.HieloPanel);
            app.StandarCheckBox_2.ValueChangedFcn = createCallbackFcn(app, @StandarCheckBox_2ValueChanged, true);
            app.StandarCheckBox_2.Enable = 'off';
            app.StandarCheckBox_2.Text = 'Standar';
            app.StandarCheckBox_2.Position = [225 91 64 22];
            app.StandarCheckBox_2.Value = true;

            % Create EspesormmEditFieldLabel
            app.EspesormmEditFieldLabel = uilabel(app.HieloPanel);
            app.EspesormmEditFieldLabel.HorizontalAlignment = 'right';
            app.EspesormmEditFieldLabel.Position = [11 18 79 22];
            app.EspesormmEditFieldLabel.Text = 'Espesor [mm]';

            % Create EspesormmEditField
            app.EspesormmEditField = uieditfield(app.HieloPanel, 'numeric');
            app.EspesormmEditField.Limits = [0 Inf];
            app.EspesormmEditField.Enable = 'off';
            app.EspesormmEditField.Position = [105 18 100 22];
            app.EspesormmEditField.Value = 2;

            % Create PesoCheckBox
            app.PesoCheckBox = uicheckbox(app.HieloPanel);
            app.PesoCheckBox.ValueChangedFcn = createCallbackFcn(app, @PesoCheckBoxValueChanged, true);
            app.PesoCheckBox.Enable = 'off';
            app.PesoCheckBox.Text = 'Peso';
            app.PesoCheckBox.Position = [225 52 49 22];

            % Create ActivarButton_2
            app.ActivarButton_2 = uibutton(app.HieloPanel, 'state');
            app.ActivarButton_2.ValueChangedFcn = createCallbackFcn(app, @ActivarButton_2ValueChanged, true);
            app.ActivarButton_2.Text = 'Activar';
            app.ActivarButton_2.Position = [24 90 100 22];

            % Create Panel
            app.Panel = uipanel(app.HiptesisTab);
            app.Panel.Title = 'Panel';
            app.Panel.Position = [58 519 330 153];

            % Create TemperaturaCLabel
            app.TemperaturaCLabel = uilabel(app.Panel);
            app.TemperaturaCLabel.HorizontalAlignment = 'right';
            app.TemperaturaCLabel.Position = [3 91 96 22];
            app.TemperaturaCLabel.Text = 'Temperatura [°C]';

            % Create TemperaturaCEditField
            app.TemperaturaCEditField = uieditfield(app.Panel, 'numeric');
            app.TemperaturaCEditField.Position = [114 91 100 22];

            % Create Label
            app.Label = uilabel(app.HiptesisTab);
            app.Label.FontColor = [0.8 0.8 0.8];
            app.Label.Position = [90 8 595 22];
            app.Label.Text = '* La hiptesis adicional asume unicamente una carga de viento en Zona B con -10 °c y en Zona C con -15 °C.';

            % Create Tabladetendido
            app.Tabladetendido = uitab(app.TabGroup);
            app.Tabladetendido.Title = 'Tabla de tendido';

            % Create UITable3
            app.UITable3 = uitable(app.Tabladetendido);
            app.UITable3.ColumnName = {'Column 1'; 'Column 2'; 'Column 3'; 'Column 4'};
            app.UITable3.RowName = {};
            app.UITable3.Position = [21 29 1100 398];

            % Create TemperaturamnimaminEditFieldLabel
            app.TemperaturamnimaminEditFieldLabel = uilabel(app.Tabladetendido);
            app.TemperaturamnimaminEditFieldLabel.HorizontalAlignment = 'right';
            app.TemperaturamnimaminEditFieldLabel.Position = [41 647 147 22];
            app.TemperaturamnimaminEditFieldLabel.Text = 'Temperatura mínima Θmin';

            % Create TemperaturamnimaminEditField
            app.TemperaturamnimaminEditField = uieditfield(app.Tabladetendido, 'numeric');
            app.TemperaturamnimaminEditField.ValueChangedFcn = createCallbackFcn(app, @TemperaturamnimaminEditFieldValueChanged, true);
            app.TemperaturamnimaminEditField.Position = [203 647 100 22];
            app.TemperaturamnimaminEditField.Value = -20;

            % Create TemperaturamximamaxEditFieldLabel
            app.TemperaturamximamaxEditFieldLabel = uilabel(app.Tabladetendido);
            app.TemperaturamximamaxEditFieldLabel.HorizontalAlignment = 'right';
            app.TemperaturamximamaxEditFieldLabel.Position = [341 647 154 22];
            app.TemperaturamximamaxEditFieldLabel.Text = 'Temperatura máxima Θmax';

            % Create TemperaturamximamaxEditField
            app.TemperaturamximamaxEditField = uieditfield(app.Tabladetendido, 'numeric');
            app.TemperaturamximamaxEditField.ValueChangedFcn = createCallbackFcn(app, @TemperaturamximamaxEditFieldValueChanged, true);
            app.TemperaturamximamaxEditField.Position = [510 647 100 22];
            app.TemperaturamximamaxEditField.Value = 75;

            % Create IntervalosdetemperaturaSpinnerLabel
            app.IntervalosdetemperaturaSpinnerLabel = uilabel(app.Tabladetendido);
            app.IntervalosdetemperaturaSpinnerLabel.HorizontalAlignment = 'right';
            app.IntervalosdetemperaturaSpinnerLabel.Position = [641 647 143 22];
            app.IntervalosdetemperaturaSpinnerLabel.Text = 'Intervalos de temperatura';

            % Create IntervalosdetemperaturaSpinner
            app.IntervalosdetemperaturaSpinner = uispinner(app.Tabladetendido);
            app.IntervalosdetemperaturaSpinner.Limits = [0 Inf];
            app.IntervalosdetemperaturaSpinner.Position = [799 647 100 22];
            app.IntervalosdetemperaturaSpinner.Value = 5;

            % Create TemperaturaLabel
            app.TemperaturaLabel = uilabel(app.Tabladetendido);
            app.TemperaturaLabel.FontWeight = 'bold';
            app.TemperaturaLabel.Position = [31 677 98 22];
            app.TemperaturaLabel.Text = 'Temperatura (Θ)';

            % Create VanoaLabel
            app.VanoaLabel = uilabel(app.Tabladetendido);
            app.VanoaLabel.FontWeight = 'bold';
            app.VanoaLabel.Position = [31 607 53 22];
            app.VanoaLabel.Text = 'Vano (a)';

            % Create VanomnimoEditFieldLabel
            app.VanomnimoEditFieldLabel = uilabel(app.Tabladetendido);
            app.VanomnimoEditFieldLabel.HorizontalAlignment = 'right';
            app.VanomnimoEditFieldLabel.Position = [111 585 79 22];
            app.VanomnimoEditFieldLabel.Text = 'Vano mínimo ';

            % Create VanomnimoEditField
            app.VanomnimoEditField = uieditfield(app.Tabladetendido, 'numeric');
            app.VanomnimoEditField.Limits = [0 Inf];
            app.VanomnimoEditField.ValueChangedFcn = createCallbackFcn(app, @VanomnimoEditFieldValueChanged, true);
            app.VanomnimoEditField.Position = [205 585 100 22];
            app.VanomnimoEditField.Value = 50;

            % Create VanomximoEditFieldLabel
            app.VanomximoEditFieldLabel = uilabel(app.Tabladetendido);
            app.VanomximoEditFieldLabel.HorizontalAlignment = 'right';
            app.VanomximoEditFieldLabel.Position = [421 585 78 22];
            app.VanomximoEditFieldLabel.Text = 'Vano máximo';

            % Create VanomximoEditField
            app.VanomximoEditField = uieditfield(app.Tabladetendido, 'numeric');
            app.VanomximoEditField.Limits = [0 Inf];
            app.VanomximoEditField.ValueChangedFcn = createCallbackFcn(app, @VanomximoEditFieldValueChanged, true);
            app.VanomximoEditField.Position = [514 585 100 22];
            app.VanomximoEditField.Value = 600;

            % Create IntervaloentrevanosSpinnerLabel
            app.IntervaloentrevanosSpinnerLabel = uilabel(app.Tabladetendido);
            app.IntervaloentrevanosSpinnerLabel.HorizontalAlignment = 'right';
            app.IntervaloentrevanosSpinnerLabel.Position = [671 585 118 22];
            app.IntervaloentrevanosSpinnerLabel.Text = 'Intervalo entre vanos';

            % Create IntervaloentrevanosSpinner
            app.IntervaloentrevanosSpinner = uispinner(app.Tabladetendido);
            app.IntervaloentrevanosSpinner.Step = 5;
            app.IntervaloentrevanosSpinner.Limits = [0 Inf];
            app.IntervaloentrevanosSpinner.Position = [804 585 100 22];
            app.IntervaloentrevanosSpinner.Value = 25;

            % Create GenerartablaButton
            app.GenerartablaButton = uibutton(app.Tabladetendido, 'push');
            app.GenerartablaButton.ButtonPushedFcn = createCallbackFcn(app, @GenerartablaButtonPushed, true);
            app.GenerartablaButton.Position = [1013 446 100 22];
            app.GenerartablaButton.Text = 'Generar tabla';

            % Create TemperaturaenCLabel
            app.TemperaturaenCLabel = uilabel(app.Tabladetendido);
            app.TemperaturaenCLabel.Position = [771 617 111 22];
            app.TemperaturaenCLabel.Text = '*Temperatura en °C';

            % Create DistanciasenmetrosLabel
            app.DistanciasenmetrosLabel = uilabel(app.Tabladetendido);
            app.DistanciasenmetrosLabel.Position = [771 555 123 22];
            app.DistanciasenmetrosLabel.Text = '*Distancias en metros';

            % Create UITable3_2
            app.UITable3_2 = uitable(app.Tabladetendido);
            app.UITable3_2.ColumnName = {'Hipótesis'; 'Temperatura °C'; 'Carga Viento [kg/m]'; 'Carga Hielo [kg/m]'; 'Tensión [m]'; 'Segurity coef'; 'overload coef'; 'Flecha vertical [m]'; 'Flecha inclinada [m]'; 'Obervaciones'};
            app.UITable3_2.RowName = {};
            app.UITable3_2.Position = [21 479 1100 46];

            % Create HiptesismsdesfavorableLabel
            app.HiptesismsdesfavorableLabel = uilabel(app.Tabladetendido);
            app.HiptesismsdesfavorableLabel.FontWeight = 'bold';
            app.HiptesismsdesfavorableLabel.Position = [31 537 164 22];
            app.HiptesismsdesfavorableLabel.Text = 'Hipótesis más desfavorable';

            % Create GrficasTab
            app.GrficasTab = uitab(app.TabGroup);
            app.GrficasTab.Title = 'Gráficas';

            % Create UIAxes
            app.UIAxes = uiaxes(app.GrficasTab);
            title(app.UIAxes, 'Flechas')
            xlabel(app.UIAxes, 'X')
            ylabel(app.UIAxes, 'Y')
            app.UIAxes.XGrid = 'on';
            app.UIAxes.XMinorGrid = 'on';
            app.UIAxes.YGrid = 'on';
            app.UIAxes.YMinorGrid = 'on';
            app.UIAxes.Position = [339 104 690 530];

            % Create DatosusadosenelclculoLabel
            app.DatosusadosenelclculoLabel = uilabel(app.GrficasTab);
            app.DatosusadosenelclculoLabel.FontWeight = 'bold';
            app.DatosusadosenelclculoLabel.Position = [86 647 160 22];
            app.DatosusadosenelclculoLabel.Text = 'Datos usados en el cálculo';

            % Create ResultadosLabel
            app.ResultadosLabel = uilabel(app.GrficasTab);
            app.ResultadosLabel.FontWeight = 'bold';
            app.ResultadosLabel.Position = [86 454 70 22];
            app.ResultadosLabel.Text = 'Resultados';

            % Create AplicarButton
            app.AplicarButton = uibutton(app.GrficasTab, 'push');
            app.AplicarButton.ButtonPushedFcn = createCallbackFcn(app, @AplicarButtonPushed, true);
            app.AplicarButton.Position = [170 477 100 22];
            app.AplicarButton.Text = 'Aplicar';

            % Create VanomSpinnerLabel
            app.VanomSpinnerLabel = uilabel(app.GrficasTab);
            app.VanomSpinnerLabel.HorizontalAlignment = 'center';
            app.VanomSpinnerLabel.Position = [98 609 53 22];
            app.VanomSpinnerLabel.Text = 'Vano [m]';

            % Create VanomSpinner
            app.VanomSpinner = uispinner(app.GrficasTab);
            app.VanomSpinner.Step = 5;
            app.VanomSpinner.ValueChangingFcn = createCallbackFcn(app, @VanomSpinnerValueChanging, true);
            app.VanomSpinner.Limits = [1e-08 Inf];
            app.VanomSpinner.Position = [175 609 100 22];
            app.VanomSpinner.Value = 1e-08;

            % Create TensinvrticekgSpinnerLabel
            app.TensinvrticekgSpinnerLabel = uilabel(app.GrficasTab);
            app.TensinvrticekgSpinnerLabel.HorizontalAlignment = 'center';
            app.TensinvrticekgSpinnerLabel.Position = [98 564 64 27];
            app.TensinvrticekgSpinnerLabel.Text = {'Tensión '; 'vértice [kg]'};

            % Create TensinvrticekgSpinner
            app.TensinvrticekgSpinner = uispinner(app.GrficasTab);
            app.TensinvrticekgSpinner.Step = 5;
            app.TensinvrticekgSpinner.ValueChangingFcn = createCallbackFcn(app, @TensinvrticekgSpinnerValueChanging, true);
            app.TensinvrticekgSpinner.Limits = [1e-08 Inf];
            app.TensinvrticekgSpinner.Position = [175 569 100 22];
            app.TensinvrticekgSpinner.Value = 1e-08;

            % Create PesoaparentekgkmSpinnerLabel
            app.PesoaparentekgkmSpinnerLabel = uilabel(app.GrficasTab);
            app.PesoaparentekgkmSpinnerLabel.HorizontalAlignment = 'center';
            app.PesoaparentekgkmSpinnerLabel.Position = [77 518 86 28];
            app.PesoaparentekgkmSpinnerLabel.Text = {'Peso aparente '; '[kg/km]'};

            % Create PesoaparentekgkmSpinner
            app.PesoaparentekgkmSpinner = uispinner(app.GrficasTab);
            app.PesoaparentekgkmSpinner.Step = 5;
            app.PesoaparentekgkmSpinner.ValueChangingFcn = createCallbackFcn(app, @PesoaparentekgkmSpinnerValueChanging, true);
            app.PesoaparentekgkmSpinner.Limits = [1e-08 Inf];
            app.PesoaparentekgkmSpinner.Position = [175 524 100 22];
            app.PesoaparentekgkmSpinner.Value = 1e-08;

            % Create FlechamEditFieldLabel
            app.FlechamEditFieldLabel = uilabel(app.GrficasTab);
            app.FlechamEditFieldLabel.HorizontalAlignment = 'right';
            app.FlechamEditFieldLabel.Position = [97 420 62 22];
            app.FlechamEditFieldLabel.Text = 'Flecha [m]';

            % Create FlechamEditField
            app.FlechamEditField = uieditfield(app.GrficasTab, 'numeric');
            app.FlechamEditField.Limits = [1e-08 Inf];
            app.FlechamEditField.Editable = 'off';
            app.FlechamEditField.Position = [174 420 100 22];
            app.FlechamEditField.Value = 1e-08;

            % Create LongituddeconductormEditFieldLabel
            app.LongituddeconductormEditFieldLabel = uilabel(app.GrficasTab);
            app.LongituddeconductormEditFieldLabel.HorizontalAlignment = 'right';
            app.LongituddeconductormEditFieldLabel.Position = [81 368 78 27];
            app.LongituddeconductormEditFieldLabel.Text = {'Longitud de '; 'conductor [m]'};

            % Create LongituddeconductormEditField
            app.LongituddeconductormEditField = uieditfield(app.GrficasTab, 'numeric');
            app.LongituddeconductormEditField.Limits = [1e-08 Inf];
            app.LongituddeconductormEditField.Editable = 'off';
            app.LongituddeconductormEditField.Position = [174 373 100 22];
            app.LongituddeconductormEditField.Value = 1e-08;

            % Create FlechasmximasymnimasCheckBox
            app.FlechasmximasymnimasCheckBox = uicheckbox(app.GrficasTab);
            app.FlechasmximasymnimasCheckBox.ValueChangedFcn = createCallbackFcn(app, @FlechasmximasymnimasCheckBoxValueChanged, true);
            app.FlechasmximasymnimasCheckBox.Text = {'Flechas máximas'; ' y mínimas'};
            app.FlechasmximasymnimasCheckBox.Position = [161 321 116 28];

            % Create ParaunpesodeLabel
            app.ParaunpesodeLabel = uilabel(app.GrficasTab);
            app.ParaunpesodeLabel.Position = [100 268 181 22];
            app.ParaunpesodeLabel.Text = 'Para un peso de:';

            % Create FlechamximaLabel
            app.FlechamximaLabel = uilabel(app.GrficasTab);
            app.FlechamximaLabel.Position = [100 237 181 22];
            app.FlechamximaLabel.Text = 'Flecha máxima: ';

            % Create PesoaparenteLabel_2
            app.PesoaparenteLabel_2 = uilabel(app.GrficasTab);
            app.PesoaparenteLabel_2.Position = [100 207 181 22];
            app.PesoaparenteLabel_2.Text = 'Peso aparente:';

            % Create FlechamnimaLabel
            app.FlechamnimaLabel = uilabel(app.GrficasTab);
            app.FlechamnimaLabel.Position = [101 127 181 22];
            app.FlechamnimaLabel.Text = 'Flecha mínima: ';

            % Create LongitudmnimaLabel_2
            app.LongitudmnimaLabel_2 = uilabel(app.GrficasTab);
            app.LongitudmnimaLabel_2.Position = [101 97 181 22];
            app.LongitudmnimaLabel_2.Text = 'Longitud mínima: ';

            % Create MximaflechaLabel
            app.MximaflechaLabel = uilabel(app.GrficasTab);
            app.MximaflechaLabel.FontWeight = 'bold';
            app.MximaflechaLabel.Position = [90 288 91 22];
            app.MximaflechaLabel.Text = 'Máxima flecha:';

            % Create MnimaflechaLabel
            app.MnimaflechaLabel = uilabel(app.GrficasTab);
            app.MnimaflechaLabel.FontWeight = 'bold';
            app.MnimaflechaLabel.Position = [90 187 89 22];
            app.MnimaflechaLabel.Text = 'Mínima flecha:';

            % Create PesoaparenteLabel
            app.PesoaparenteLabel = uilabel(app.GrficasTab);
            app.PesoaparenteLabel.Position = [101 157 181 22];
            app.PesoaparenteLabel.Text = 'Peso aparente:';

            % Create InputTab
            app.InputTab = uitab(app.TabGroup);
            app.InputTab.Title = 'Input';
            app.InputTab.Interruptible = 'off';

            % Create UITable
            app.UITable = uitable(app.InputTab);
            app.UITable.ColumnName = {'Nickname'; 'Diámetro del cable [mm]'; 'Secciones Transveersal Total [mm²]'; 'Módulo de elasticidad [kg/mm²]'; 'Coeficiente de dilatación'; 'Pesos Total [kg/km]'; 'Carga de rotura [kg]'; 'Tipo'};
            app.UITable.RowName = {};
            app.UITable.CellSelectionCallback = createCallbackFcn(app, @UITableCellSelection, true);
            app.UITable.Position = [31 19 1100 530];

            % Create SeleccioneelnombredelconductordelossiguientetablaLabel
            app.SeleccioneelnombredelconductordelossiguientetablaLabel = uilabel(app.InputTab);
            app.SeleccioneelnombredelconductordelossiguientetablaLabel.Position = [41 568 320 22];
            app.SeleccioneelnombredelconductordelossiguientetablaLabel.Text = 'Seleccione el nombre del conductor de los siguiente tabla:';

            % Create LoadDataButton
            app.LoadDataButton = uibutton(app.InputTab, 'push');
            app.LoadDataButton.ButtonPushedFcn = createCallbackFcn(app, @LoadDataButtonPushed, true);
            app.LoadDataButton.Visible = 'off';
            app.LoadDataButton.Position = [851 567 100 22];
            app.LoadDataButton.Text = 'Load Data';

            % Create UITable_2
            app.UITable_2 = uitable(app.InputTab);
            app.UITable_2.ColumnName = {'Nickname'; 'Diámetro del cable [mm]'; 'Secciones Transveersal Total [mm²]'; 'Módulo de elasticidad [kg/mm²]'; 'Coeficiente de dilatación'; 'Pesos Total [kg/km]'; 'Carga de rotura [kg]'; 'Tipo'};
            app.UITable_2.RowName = {};
            app.UITable_2.Position = [31 625 1100 44];

            % Create ConductorseleccionadoLabel
            app.ConductorseleccionadoLabel = uilabel(app.InputTab);
            app.ConductorseleccionadoLabel.FontWeight = 'bold';
            app.ConductorseleccionadoLabel.Position = [50 678 145 22];
            app.ConductorseleccionadoLabel.Text = 'Conductor seleccionado';

            % Create CalcularcasodeestudioButton
            app.CalcularcasodeestudioButton = uibutton(app.InputTab, 'push');
            app.CalcularcasodeestudioButton.ButtonPushedFcn = createCallbackFcn(app, @CalcularcasodeestudioButtonPushed, true);
            app.CalcularcasodeestudioButton.Position = [981 567 147 22];
            app.CalcularcasodeestudioButton.Text = 'Calcular caso de estudio';

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = LIMECAL_exported

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.UIFigure)

            % Execute the startup function
            runStartupFcn(app, @startupFcn)

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.UIFigure)
        end
    end
end